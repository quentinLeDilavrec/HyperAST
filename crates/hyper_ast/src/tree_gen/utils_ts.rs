use crate::tree_gen::parser::Node;

pub use super::TsEnableTS;
pub use super::TsType;
pub fn tree_sitter_parse(
    text: &[u8],
    language: &tree_sitter::Language,
) -> Result<tree_sitter::Tree, tree_sitter::Tree> {
    let mut parser = tree_sitter::Parser::new();
    // TODO see if a timeout of a cancellation flag could be useful
    // const MINUTE: u64 = 60 * 1000 * 1000;
    // parser.set_timeout_micros(timeout_micros);
    // parser.set_cancellation_flag(flag);
    parser.set_language(language).unwrap();
    let tree = parser.parse(text, None).unwrap();
    if tree.root_node().has_error() {
        Err(tree)
    } else {
        Ok(tree)
    }
}

use super::parser::Visibility;

#[repr(C)]
#[derive(Debug, Copy, Clone)]
#[allow(dead_code)] // NOTE: created by tree sitter
pub(crate) enum TreeCursorStep {
    TreeCursorStepNone,
    TreeCursorStepHidden,
    TreeCursorStepVisible,
}

impl TreeCursorStep {
    pub(crate) fn ok(&self) -> Option<Visibility> {
        match self {
            TreeCursorStep::TreeCursorStepNone => None,
            TreeCursorStep::TreeCursorStepHidden => Some(Visibility::Hidden),
            TreeCursorStep::TreeCursorStepVisible => Some(Visibility::Visible),
        }
    }
}

unsafe extern "C" {
    fn ts_tree_cursor_goto_first_child_internal(
        self_: *mut tree_sitter::ffi::TSTreeCursor,
    ) -> TreeCursorStep;
    fn ts_tree_cursor_goto_next_sibling_internal(
        self_: *mut tree_sitter::ffi::TSTreeCursor,
    ) -> TreeCursorStep;

}

// typedef struct {
//   const TSTree *tree;
//   Array(TreeCursorEntry) stack;
//   TSSymbol root_alias_symbol;
// } TreeCursor;
#[repr(C)]
struct TreeCursor {
    pub tree: *const tree_sitter::ffi::TSTree,
    pub stack: Array<TreeCursorEntry>,
    pub root_alias_symbol: tree_sitter::ffi::TSSymbol,
}

type TreeCursorEntry = ();

// Array(T)       \
//   struct {             \
//     T *contents;       \
//     uint32_t size;     \
//     uint32_t capacity; \
//   }
#[repr(C)]
struct Array<T> {
    pub contents: *mut T,
    pub size: u32,
    pub capacity: u32,
}

#[repr(transparent)]
pub struct TNode<'a>(pub tree_sitter::Node<'a>);

impl crate::tree_gen::parser::Node for TNode<'_> {
    fn kind(&self) -> &str {
        self.0.kind()
    }

    fn start_byte(&self) -> usize {
        self.0.start_byte()
    }

    fn end_byte(&self) -> usize {
        self.0.end_byte()
    }

    fn child_count(&self) -> usize {
        self.0.child_count()
    }

    fn child(&self, i: usize) -> Option<Self> {
        self.0.child(i).map(TNode)
    }

    fn is_named(&self) -> bool {
        self.0.is_named()
    }

    fn is_missing(&self) -> bool {
        self.0.is_missing()
    }

    fn is_error(&self) -> bool {
        self.0.is_error()
    }
}

impl crate::tree_gen::parser::NodeWithU16TypeId for TNode<'_> {
    fn kind_id(&self) -> u16 {
        self.0.kind_id()
    }
}

#[repr(transparent)]
#[derive(Clone)]
pub struct TTreeCursor<'a, const HIDDEN_NODES: bool = false>(pub tree_sitter::TreeCursor<'a>);

impl<const HIDDEN_NODES: bool> std::fmt::Debug for TTreeCursor<'_, HIDDEN_NODES> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_tuple("TTreeCursor")
            .field(&self.0.node().kind())
            .finish()
    }
}

impl<'a, const HIDDEN_NODES: bool> crate::tree_gen::parser::TreeCursor
    for TTreeCursor<'a, HIDDEN_NODES>
{
    type N = TNode<'a>;
    fn node(&self) -> TNode<'a> {
        TNode(self.0.node())
    }

    fn role(&self) -> Option<std::num::NonZeroU16> {
        self.0.field_id()
    }

    fn goto_parent(&mut self) -> bool {
        self.goto_parent_extended().is_some()
    }

    fn goto_parent_extended(&mut self) -> Option<Visibility> {
        if HIDDEN_NODES {
            // dbg!(self.0.depth());
            // dbg!(self.0.node().kind());
            unsafe {
                let s: &mut tree_sitter::TreeCursor<'a> = &mut self.0;
                // let s: *mut tree_sitter::ffi::TSTreeCursor = std::mem::transmute(s);
                let s: *mut TreeCursor = std::mem::transmute(s);
                if (*s).stack.size <= 1 {
                    TreeCursorStep::TreeCursorStepNone
                } else {
                    log::trace!("prepro2 goto parent then {:?}", self.0.node().kind());
                    // dbg!(self.0.depth());
                    // dbg!((*s).stack.size);
                    // dbg!(self.0.node().kind());
                    (*s).stack.size -= 1;
                    // dbg!(self.0.depth());
                    // dbg!(self.0.node().kind());
                    log::trace!("prepro2 goto parent now {:?}", self.0.node().kind());
                    // WARN is should call ts_tree_cursor_is_entry_visible(s, ...)
                    TreeCursorStep::TreeCursorStepHidden
                }
            }
            .ok()
        } else if self.0.goto_parent() {
            Some(Visibility::Visible)
        } else {
            None
        }
    }

    fn goto_first_child(&mut self) -> bool {
        self.goto_first_child_extended().is_some()
    }

    fn goto_next_sibling(&mut self) -> bool {
        self.goto_next_sibling_extended().is_some()
    }

    fn goto_first_child_extended(&mut self) -> Option<Visibility> {
        if HIDDEN_NODES {
            unsafe {
                let s = &mut self.0;
                let s: *mut tree_sitter::ffi::TSTreeCursor = std::mem::transmute(s);
                // // dbg!(self.0.node().kind());
                // let r = dbg!(ts_tree_cursor_goto_first_child_internal(s));
                // // dbg!(self.0.node().kind());
                // r
                ts_tree_cursor_goto_first_child_internal(s)
            }
            .ok()
        } else if self.0.goto_first_child() {
            Some(Visibility::Visible)
        } else {
            None
        }
    }

    fn goto_next_sibling_extended(&mut self) -> Option<Visibility> {
        if HIDDEN_NODES {
            unsafe {
                let s = &mut self.0;
                let s: *mut tree_sitter::ffi::TSTreeCursor = std::mem::transmute(s);
                // dbg!(self.0.node().kind());
                // let r = dbg!(ts_tree_cursor_goto_next_sibling_internal(s));
                // dbg!(self.0.node().kind());
                // r
                ts_tree_cursor_goto_next_sibling_internal(s)
            }
            .ok()
        } else if self.0.goto_next_sibling() {
            Some(Visibility::Visible)
        } else {
            None
        }
    }
}

/// Guaranteed to work even when considering hidden nodes,
/// i.e., goto_next_children() skips hidden parents...
pub struct PrePost<C> {
    has: super::zipped::Has,
    stack: Vec<C>,
    vis: bitvec::vec::BitVec,
}

impl<C: super::parser::TreeCursor + Clone> PrePost<C> {
    pub fn new(cursor: &C) -> Self {
        use bitvec::prelude::Lsb0;
        let mut vis = bitvec::bitvec![];
        let v = Visibility::Hidden;
        vis.push(v == Visibility::Hidden);
        Self {
            has: super::zipped::Has::Down,
            stack: vec![cursor.clone()],
            vis,
        }
    }

    pub fn current(&mut self) -> (Option<&C>, &mut super::zipped::Has) {
        (self.stack.last(), &mut self.has)
    }

    pub fn next(&mut self) -> Option<Visibility> {
        use super::zipped::Has;
        use crate::tree_gen::parser::Node;
        if self.vis.is_empty() {
            return None;
        };
        let Some(cursor) = self.stack.last_mut() else {
            return None;
        };
        let mut cursor = cursor.clone();
        if self.has != Has::Up {
            if let Some(visibility) = cursor.goto_first_child_extended() {
                self.stack.push(cursor);
                self.has = Has::Down;
                self.vis.push(visibility == Visibility::Hidden);
                return Some(visibility);
            }
        }
        if let Some(visibility) = cursor.goto_next_sibling_extended() {
            let _ = self.stack.pop().unwrap();
            let c = self.stack.last_mut().unwrap();
            if c.node().end_byte() <= cursor.node().start_byte() {
                self.has = Has::Up;
                let vis = if *self.vis.last().unwrap() {
                    Visibility::Hidden
                } else {
                    Visibility::Visible
                };
                return Some(vis);
            }
            self.stack.push(cursor);
            self.vis.push(visibility == Visibility::Hidden);
            self.has = Has::Right;
            Some(visibility)
        } else if let Some(c) = self.stack.pop() {
            self.has = Has::Up;
            if self.stack.is_empty() {
                self.stack.push(c);
                None
                // depends on usage
                // let vis = if self.vis.pop().unwrap() {
                //     Visibility::Hidden
                // } else {
                //     Visibility::Visible
                // };
                // Some(vis)
            } else {
                let vis = if *self.vis.last().unwrap() {
                    Visibility::Hidden
                } else {
                    Visibility::Visible
                };
                Some(vis)
            }
        } else {
            None
        }
    }
}

/// Guaranteed to work even when considering hidden nodes,
/// now it directly uses a goto_parent_extended I implemented,
/// so no need to keep a stack of cursors !
/// NOTE from profiling it was taking about 30% of the runtime.
pub struct PrePost2<C> {
    #[doc(hidden)]
    pub has: super::zipped::Has,
    pub cursor: C,
    pub stack: Vec<usize>,
    vis: bitvec::vec::BitVec,
    waiting: Option<Visibility>,
}

impl<C: super::parser::TreeCursor> PrePost2<C> {
    pub fn new(cursor: C) -> Self {
        use bitvec::prelude::Lsb0;
        let mut vis = bitvec::bitvec![];
        vis.push(Visibility::Visible == Visibility::Hidden);
        let mut stack = vec![cursor.node().end_byte()];
        stack.reserve(30);
        Self {
            has: super::zipped::Has::Down,
            cursor,
            stack,
            vis,
            waiting: None,
        }
    }
}

impl<C: super::parser::TreeCursor> PrePost2<C> {
    pub fn current(&mut self) -> (Option<&C>, &mut super::zipped::Has) {
        (Some(&self.cursor), &mut self.has)
    }

    pub fn next(&mut self) -> Option<Visibility> {
        use super::zipped::Has;
        if self.vis.is_empty() {
            return None;
        };
        assert_eq!(self.stack.len(), self.vis.len());
        if self.has != Has::Up {
            if let Some(visibility) = self.cursor.goto_first_child_extended() {
                let node = self.cursor.node();
                self.stack.push(node.end_byte());
                self.has = Has::Down;
                self.vis.push(visibility == Visibility::Hidden);
                return Some(visibility);
            }
        }

        let _ = self.stack.pop().unwrap();
        let _ = self.vis.pop().unwrap();
        // TODO check if it could be relaxed to self.has != Has::Down
        if self.has == Has::Up && self.waiting.is_some() && !self.stack.is_empty() {
            let node = self.cursor.node();
            if *self.stack.last().unwrap() <= node.start_byte()
                && node.start_byte() < node.end_byte()
            {
                self.has = Has::Up;
                let vis = if *self.vis.last().unwrap() {
                    Visibility::Hidden
                } else {
                    Visibility::Visible
                };
                Some(vis)
            } else {
                let vis = self.waiting.take().unwrap();
                self.has = Has::Right;
                self.stack.push(node.end_byte());
                self.vis.push(vis == Visibility::Hidden);
                Some(vis)
            }
        } else if let Some(visibility) = self.cursor.goto_next_sibling_extended() {
            let node = self.cursor.node();
            if *self.stack.last().unwrap() <= node.start_byte()
                && node.start_byte() < node.end_byte()
            {
                self.waiting = Some(visibility);
                self.has = Has::Up;
                let vis = if *self.vis.last().unwrap() {
                    Visibility::Hidden
                } else {
                    Visibility::Visible
                };
                return Some(vis);
            }
            self.stack.push(node.end_byte());
            self.vis.push(visibility == Visibility::Hidden);
            assert_eq!(self.stack.len(), self.vis.len());
            self.has = Has::Right;
            Some(visibility)
        } else if let Some(_) = self.cursor.goto_parent_extended() {
            // NOTE do not need the visibility
            // I don't have a good way of getting the correct one anyway
            self.has = Has::Up;
            if let Some(vis) = self.vis.last() {
                let vis = if *vis {
                    Visibility::Hidden
                } else {
                    Visibility::Visible
                };
                Some(vis)
            } else {
                None
            }
        } else if let Some(vis) = self.vis.last() {
            let vis = if *vis {
                Visibility::Hidden
            } else {
                Visibility::Visible
            };
            Some(vis)
        } else {
            None
        }
    }
}

#[cfg(feature = "legion")]
pub fn make_leaf<TS>(
    node_store: &mut crate::store::nodes::legion::NodeStoreInner,
    label_store: &mut crate::store::labels::LabelStore,
    dedup: &mut crate::store::nodes::legion::DedupInner,
    line_break: &[u8],
    kind: TS::Ty,
    text: &str,
    f: impl FnOnce(&mut crate::store::nodes::legion::dyn_builder::EntityBuilder),
) -> (
    crate::store::nodes::legion::NodeIdentifier,
    super::SubTreeMetrics<crate::hashed::SyntaxNodeHashs<u32>>,
)
where
    TS: crate::types::TypeStore + crate::types::ETypeStore,
{
    let bytes_len = text.len();
    let line_count = text
        .matches(std::str::from_utf8(line_break).expect("use a proper utf8 line break"))
        .count();
    let line_count = num::ToPrimitive::to_u32(&line_count).expect("too many newlines");
    use crate::types::LabelStore;
    let text_id = label_store.get_or_insert(text);
    use crate::hashed::IndexingHashBuilder;
    let hbuilder: crate::hashed::HashesBuilder<crate::hashed::SyntaxNodeHashs<u32>> =
        crate::hashed::HashesBuilder::new(Default::default(), &kind, &text, 1);
    let hsyntax = hbuilder.most_discriminating();
    let hashable = &hsyntax;
    let eq = |x: crate::store::nodes::legion::EntryRef| {
        let t = x.get_component::<TS::Ty>();
        if t != Ok(&kind) {
            return false;
        }
        let l = x.get_component::<crate::store::defaults::LabelIdentifier>();
        if l != Ok(&text_id) {
            return false;
        }
        // NOTE no children expected
        true
    };
    let insertion = node_store.prepare_insertion(dedup, &hashable, eq);
    use crate::hashed::MetaDataHashsBuilder;
    let mut hashs = hbuilder.build();
    hashs.structt = 0;
    hashs.label = 0;
    let compressed_node = if let Some(id) = insertion.occupied_id() {
        id
    } else {
        use crate::store::nodes::compo;
        let vacant = insertion.vacant();
        let mut dyn_builder = crate::store::nodes::legion::subtree_builder::<TS>(kind);
        dyn_builder.add(compo::BytesLen(bytes_len.try_into().unwrap()));
        dyn_builder.add(text_id);
        dyn_builder.add(hashs);
        dyn_builder.add(crate::filter::BloomSize::None);
        if line_count != 0 {
            dyn_builder.add(compo::LineCount(line_count));
        }
        f(&mut dyn_builder);
        vacant.insert_built(dyn_builder.build())
    };
    let metrics = crate::tree_gen::SubTreeMetrics {
        size: 1,
        height: 0,
        size_no_spaces: 0,
        hashs,
        line_count,
    };
    (compressed_node, metrics)
}
