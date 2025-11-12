use egui::TextFormat;
use epaint::text::LayoutSection;

use egui_addon::syntax_highlighting::simple::CodeTheme;
use egui_addon::syntax_highlighting::syntax_highlighting_async::cache::{
    IncrementalCache, IncrementalComputer, Spawner,
};

use super::cache;
use super::hyperast_layouter::AdvTheme;
use super::store;

use store::FetchedHyperAST;
use store::LockedFetchedHyperAST;
use store::NodeIdentifier;

use std::sync::atomic::Ordering::Relaxed as RemOrdering;

const REFRESH_DELAY: std::time::Duration = std::time::Duration::from_millis(10);

type ArcFetchedHAST = std::sync::Arc<FetchedHyperAST>;

fn subtree_to_string(store: &FetchedHyperAST, nid: NodeIdentifier) -> String {
    let read = store.read();
    let s = {
        // SAFETY: the transmuted value does not escape the function scope
        // NOTE issue with the usual widening to 'static ...
        let read: &LockedFetchedHyperAST<'_> = unsafe { std::mem::transmute(&read) };
        use hyperast::nodes::TextSerializer;
        ToString::to_string(&TextSerializer::<_, _>::new(read, nid))
    };
    drop(read);
    s
}

struct PrettyPrinter;

impl Default for PrettyPrinter {
    fn default() -> Self {
        Self {}
    }
}
impl cache::ComputerMut<(&FetchedHyperAST, NodeIdentifier), String> for PrettyPrinter {
    fn compute(&mut self, (store, id): (&FetchedHyperAST, NodeIdentifier)) -> String {
        subtree_to_string(store, id)
    }
}
type PPCache = cache::FrameCache<String, PrettyPrinter>;

#[derive(Copy, Clone, Hash)]
pub(crate) struct PPBuilder<Thm, HAST = ArcFetchedHAST, L = usize> {
    store: HAST,
    theme: Thm,
    nid: NodeIdentifier,
    len: L,
}

type Prm<'a> = PPBuilder<AdvTheme<&'a CodeTheme>, &'a ArcFetchedHAST>;

impl<Thm: std::hash::Hash> std::hash::Hash for AdvTheme<Thm> {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.theme.hash(state);
        self.size.to_bits().hash(state);
        self.fg.hash(state);
        self.bg.hash(state);
    }
}

impl<Thm: Clone> AdvTheme<&Thm> {
    pub(crate) fn cloned(&self) -> AdvTheme<Thm> {
        AdvTheme {
            theme: self.theme.clone(),
            size: self.size,
            fg: self.fg,
            bg: self.bg,
        }
    }
}
impl<Thm> AdvTheme<Thm> {
    pub(crate) fn as_ref(&self) -> AdvTheme<&Thm> {
        AdvTheme {
            theme: &self.theme,
            size: self.size,
            fg: self.fg,
            bg: self.bg,
        }
    }
}

#[derive(Default)]
struct Spawnr;
impl Spawner<Prm<'_>, Layouter> for Spawnr {
    fn spawn(&self, ctx: &egui::Context, param: Prm<'_>) -> Layouter {
        Layouter {
            ctx: ctx.clone(),
            total_str_len: param.len,
            ..Default::default()
        }
    }
}

type MacroTasks = Vec<std::sync::Arc<std::sync::Mutex<egui_addon::async_exec::TimeoutHandle>>>;

#[derive(Default)]
struct LayoutQueue {
    remaining: std::sync::atomic::AtomicUsize,
    queue: crossbeam_queue::SegQueue<Vec<LayoutSection>>,
}

#[derive(Default)]
struct Layouter {
    ctx: egui::Context,
    mt: MacroTasks,
    sections: Vec<LayoutSection>,
    queued: std::sync::Arc<LayoutQueue>,
    i: usize,
    total_str_len: usize,
}

impl IncrementalComputer<Spawnr, Prm<'_>, Vec<LayoutSection>> for Layouter {
    fn increment(&mut self, _spawner: &Spawnr, param: Prm<'_>) -> Vec<LayoutSection> {
        assert_eq!(param.len, self.total_str_len);
        if self.mt.is_empty() && self.i < self.total_str_len {
            let h = std::sync::Arc::clone(&self.queued);
            let ctx = self.ctx.clone();
            let prm = param.arc_clone();
            let t_h = egui_addon::async_exec::spawn_macrotask(Box::new(move || {
                let (len, sections) = prm.clone().compute_sync();
                h.queue.push(sections);
                h.remaining.store(len, RemOrdering);
                ctx.request_repaint_after(REFRESH_DELAY);
            }));
            let t_h = std::sync::Arc::new(std::sync::Mutex::new(t_h));
            self.mt.push(t_h);
            vec![param.section(0..self.total_str_len)]
        } else if self.i < self.total_str_len {
            self.i = self.queued.as_ref().remaining.load(RemOrdering);
            for _ in 0..self.queued.as_ref().queue.len() {
                let sections = self.queued.as_ref().queue.pop();
                if let Some(mut sections) = sections {
                    self.sections.extend_from_slice(&sections);
                }
            }
            let mut sections = self.sections.clone();
            if self.i < self.total_str_len {
                sections.push(param.section(self.i..self.total_str_len))
            }
            sections
        } else {
            self.mt.clear();
            self.sections.clone()
        }
    }
}

impl<HAST> PPBuilder<(), HAST, ()> {
    pub fn new(store: HAST, nid: NodeIdentifier) -> Self {
        PPBuilder {
            store,
            theme: (),
            nid,
            len: (),
        }
    }
    pub fn theme<Thm>(self, theme: Thm) -> PPBuilder<Thm, HAST, ()> {
        PPBuilder {
            theme,
            store: self.store,
            nid: self.nid,
            len: (),
        }
    }
}

impl PPBuilder<(), ArcFetchedHAST, ()> {
    pub fn compute_incr(&self, ui: &egui::Ui) -> egui::text::LayoutJob {
        let theme = AdvTheme::from(ui);
        PPBuilder {
            store: &self.store,
            theme: &theme,
            nid: self.nid,
            len: self.len,
        }
        .compute_incr(ui.ctx())
    }
}

impl PPBuilder<AdvTheme<CodeTheme>, ArcFetchedHAST, ()> {
    pub fn compute_incr(&self, ctx: &egui::Context) -> egui::text::LayoutJob {
        PPBuilder {
            store: &self.store,
            theme: &self.theme,
            nid: self.nid,
            len: self.len,
        }
        .compute_incr(ctx)
    }
}

type HCache = IncrementalCache<Layouter, Spawnr>;
impl PPBuilder<&AdvTheme<CodeTheme>, &ArcFetchedHAST, ()> {
    pub fn compute_incr(&self, ctx: &egui::Context) -> egui::text::LayoutJob {
        let key = (self.store.as_ref(), self.nid);
        let code = ctx.memory_mut(|mem| mem.caches.cache::<PPCache>().get(key));
        let store = std::sync::Arc::clone(&self.store);
        let key = PPBuilder {
            len: code.len(),
            store: self.store,
            theme: self.theme.as_ref(),
            nid: self.nid,
        };
        let sections = ctx.memory_mut(|mem| mem.caches.cache::<HCache>().get(ctx, key));
        epaint::text::LayoutJob {
            text: code,
            sections,
            ..epaint::text::LayoutJob::default()
        }
    }
}

impl<Thm, HAST, L> PPBuilder<AdvTheme<Thm>, HAST, L> {
    fn section(&self, byte_range: std::ops::Range<usize>) -> LayoutSection {
        LayoutSection {
            leading_space: 0.0,
            byte_range,
            format: self.format(),
        }
    }
    fn format(&self) -> TextFormat {
        let thm = &self.theme;
        TextFormat {
            font_id: egui::FontId::monospace(thm.size),
            color: thm.fg,
            background: thm.bg,
            ..Default::default()
        }
    }
}

impl Prm<'_> {
    fn arc_clone(&self) -> PPBuilder<AdvTheme<CodeTheme>> {
        PPBuilder {
            store: std::sync::Arc::clone(&self.store),
            theme: self.theme.cloned(),
            nid: self.nid,
            len: self.len,
        }
    }
}

impl PPBuilder<AdvTheme<CodeTheme>> {
    fn compute_sync(&self) -> (usize, Vec<LayoutSection>) {
        use super::hyperast_layouter::Layouter;
        let layouter = Layouter::<_, _> {
            stores: &self.store.read(),
            root: self.nid,
            root_indent: "\n",
            theme: self.theme.as_ref(),
        };

        use hyperast::nodes::IndentedAlt;
        match layouter.compute() {
            Err(IndentedAlt::FmtError) => panic!(),
            Err(IndentedAlt::NoIndent) => panic!(),
            Ok(x) => x,
        }
    }
}
