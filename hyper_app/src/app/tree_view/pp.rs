use egui::TextFormat;
use egui_addon::syntax_highlighting::syntax_highlighting_async::cache::Spawner;
use epaint::text::LayoutSection;
use std::num::NonZeroU32;
use std::sync::{Arc, atomic::AtomicUsize};
use std::time::Duration;

use egui_addon::syntax_highlighting::simple::CodeTheme;
use egui_addon::syntax_highlighting::syntax_highlighting_async;

use hyperast::nodes::{IndentedAlt, TextSerializer};

use super::cache;
use super::hyperast_layouter::Layouter;
use super::store;

use store::AcessibleFetchedHyperAST;
use store::FetchedHyperAST;
use store::NodeIdentifier;

fn subtree_to_string(store: &FetchedHyperAST, nid: NodeIdentifier) -> String {
    let read = store.read();
    let s = {
        // SAFETY: the transmuted value does not escape the function scope
        // NOTE issue with the usual widening to 'static ...
        let read: &AcessibleFetchedHyperAST<'_> = unsafe { std::mem::transmute(&read) };
        ToString::to_string(&TextSerializer::<_, _>::new(read, nid))
    };
    drop(read);
    s
}

fn subtree_to_layout(
    store: &FetchedHyperAST,
    theme: &CodeTheme,
    nid: NodeIdentifier,
) -> (usize, Vec<LayoutSection>) {
    // let read = store.read();
    match Layouter::<_, _>::new(&store.read(), nid, theme).compute() {
        Err(IndentedAlt::FmtError) => panic!(),
        Err(IndentedAlt::NoIndent) => panic!(),
        Ok(x) => x,
    }
}

pub(crate) fn make_pp_code(
    store: Arc<FetchedHyperAST>,
    ctx: &egui::Context,
    nid: NodeIdentifier,
    theme: CodeTheme,
) -> epaint::text::LayoutJob {
    #[derive(Default)]
    struct PrettyPrinter {}
    impl cache::ComputerMut<(&FetchedHyperAST, NodeIdentifier), String> for PrettyPrinter {
        fn compute(&mut self, (store, id): (&FetchedHyperAST, NodeIdentifier)) -> String {
            subtree_to_string(store, id)
        }
    }

    type PPCache = cache::FrameCache<String, PrettyPrinter>;

    let code = ctx.memory_mut(|mem| mem.caches.cache::<PPCache>().get((store.as_ref(), nid)));
    #[derive(Default)]
    struct Spawnr {}
    impl Spawner<(Arc<FetchedHyperAST>, &CodeTheme, NodeIdentifier, usize), Layouter> for Spawnr {
        fn spawn(
            &self,
            ctx: &egui::Context,
            (_store, _theme, _id, len): (Arc<FetchedHyperAST>, &CodeTheme, NodeIdentifier, usize),
        ) -> Layouter {
            Layouter {
                ctx: ctx.clone(),
                total_str_len: len,
                ..Default::default()
            }
        }
    }
    use std::sync::Mutex;
    use std::sync::atomic::Ordering;
    #[derive(Default)]
    struct Layouter {
        ctx: egui::Context,
        mt: Vec<Arc<Mutex<egui_addon::async_exec::TimeoutHandle>>>,
        sections: Vec<LayoutSection>,
        /// remaining, queue
        queued: Arc<(AtomicUsize, crossbeam_queue::SegQueue<Vec<LayoutSection>>)>,
        i: usize,
        total_str_len: usize,
    }
    impl
        syntax_highlighting_async::cache::IncrementalComputer<
            Spawnr,
            (Arc<FetchedHyperAST>, &CodeTheme, NodeIdentifier, usize),
            Vec<LayoutSection>,
        > for Layouter
    {
        fn increment(
            &mut self,
            _spawner: &Spawnr,
            (store, theme, id, len): (Arc<FetchedHyperAST>, &CodeTheme, NodeIdentifier, usize),
        ) -> Vec<LayoutSection> {
            let theme = theme.clone();
            assert_eq!(len, self.total_str_len);
            if self.mt.is_empty() && self.i < self.total_str_len {
                let h = self.queued.clone();
                let ctx = self.ctx.clone();
                let fut = move || {
                    let (len, sections) = subtree_to_layout(store.as_ref(), &theme, id);
                    h.1.push(sections);
                    h.0.store(len, Ordering::Relaxed);
                    ctx.request_repaint_after(Duration::from_millis(10));
                };
                self.mt.push(Arc::new(Mutex::new(
                    egui_addon::async_exec::spawn_macrotask(Box::new(fut)),
                )));
                vec![LayoutSection {
                    leading_space: 0.0,
                    byte_range: 0..self.total_str_len,
                    format: TextFormat {
                        font_id: egui::FontId::monospace(12.0),
                        ..Default::default()
                    },
                }]
            } else if self.i < self.total_str_len {
                self.i = self.queued.as_ref().0.load(Ordering::Relaxed);
                for _ in 0..self.queued.as_ref().1.len() {
                    let sections = self.queued.as_ref().1.pop();
                    if let Some(sections) = sections {
                        self.sections.extend_from_slice(&sections);
                    }
                }
                let mut sections = self.sections.clone();

                if self.i < self.total_str_len {
                    sections.push(LayoutSection {
                        leading_space: 0.0,
                        byte_range: self.i..self.total_str_len,
                        format: TextFormat {
                            font_id: egui::FontId::monospace(12.0),
                            ..Default::default()
                        },
                    })
                }

                sections
            } else {
                self.mt.clear();
                self.sections.clone()
            }
        }
    }

    type HCache = syntax_highlighting_async::cache::IncrementalCache<Layouter, Spawnr>;

    let sections = ctx.memory_mut(|mem| {
        mem.caches
            .cache::<HCache>()
            .get(ctx, (store.clone(), &theme, nid, code.len()))
    });

    let layout_job = epaint::text::LayoutJob {
        text: code,
        sections,
        ..epaint::text::LayoutJob::default()
    };
    layout_job
}
