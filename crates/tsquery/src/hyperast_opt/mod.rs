//! # Optimized cursor
//! nodes are only persisted when captured
use super::{Status, Symbol, TreeCursorStep};
use crate::StatusLending;
use hyperast::position::structural_pos;
use hyperast::types::{Childrn, HyperAST, LangRef, LendT, NodeStore as _, TypeStore};
use hyperast::types::{HyperASTShared, HyperType, LabelStore, Labeled, RoleStore, Tree};
use hyperast::types::{WithChildren, WithPrecompQueries, WithRoles};
use structural_pos::{CursorHead, CursorHeadMove, CursorWithPersistence, PersistedNode};

macro_rules! decomp_t {
    ($T:ty, $n:expr) => {
        <$T as TypeStore>::decompress_type($n, std::any::TypeId::of::<<$T as TypeStore>::Ty>())
    };
}

pub mod no_opt;

// pub mod hyperast_opt10;
pub mod opt3;
pub mod opt4;
pub mod opt5;
pub mod opt6_ancestors_subtree;
pub mod opt7_ancestors_hidden;
pub mod opt8_parent_error;
pub mod opt9_parent_types;
pub mod opt9_parent_types2;

#[doc(hidden)]
pub struct TreeCursor<
    'hast,
    HAST: HyperASTShared,
    P = opt9_parent_types::PersistCursor<'hast, HAST>,
> {
    pub stores: &'hast HAST,
    pub pos: P,
}

impl<'hast, HAST: HyperAST> TreeCursor<'hast, HAST> {
    pub fn new(stores: &'hast HAST, mut pos: CursorWithPersistence<HAST::IdN, HAST::Idx>) -> Self
    where
        HAST::IdN: Copy,
    {
        opt9_parent_types::tree_cursor(stores, pos)
    }
}

impl<HAST: HyperAST, P> crate::WithField for self::TreeCursor<'_, HAST, P>
where
    HAST::TS: RoleStore,
{
    type IdF = <HAST::TS as RoleStore>::IdF;
}

impl<'hast, HAST: HyperASTShared, P> self::TreeCursor<'hast, HAST, P> {
    pub fn bind(pos: P, stores: &'hast HAST) -> Self {
        Self { stores, pos }
    }
}

// pub struct TreeCursor<'hast, HAST: HyperASTShared> {
//     pub stores: &'hast HAST,
//     pub pos: CursorWithPersistence<HAST::IdN, HAST::Idx>,
//     pub p: PersistedNode<HAST::IdN, HAST::Idx>,
// }

pub struct Node<'hast, HAST: HyperASTShared> {
    pub stores: &'hast HAST,
    pub pos: PersistedNode<HAST::IdN, HAST::Idx>,
}

pub struct NodeRef<'a, 'hast, HAST: HyperASTShared> {
    pub stores: &'hast HAST,
    pub pos: structural_pos::RefNode<'a, HAST::IdN, HAST::Idx>,
}

pub struct NodeRefK<'a, 'hast, HAST: HyperAST> {
    pub stores: &'hast HAST,
    pub pos: structural_pos::RefNode<'a, HAST::IdN, HAST::Idx>,
    pub kind: <<HAST as HyperAST>::TS as TypeStore>::Ty,
}

struct ExtNodeRef<'a, 'hast, HAST: HyperASTShared> {
    pub stores: &'hast HAST,
    pub pos: structural_pos::ExtRefNode<'a, HAST::IdN, HAST::Idx>,
}

pub mod opt;

pub struct CursorStatus<IdF> {
    pub has_later_siblings: bool,
    pub has_later_named_siblings: bool,
    pub can_have_later_siblings_with_this_field: bool,
    pub field_id: IdF,
    pub supertypes: Vec<Symbol>,
}

mod impl_nodes;

mod cursor_utils;
mod cursor_utils2;
mod cursor_utils3;

use cursor_utils::SuperTypeIter;
use cursor_utils::text;
use cursor_utils::{goto_first_child_internal, goto_next_sibling_internal, goto_parent};
use cursor_utils::{has_child_with_field_id, role};
use cursor_utils::{kind, resolve};

pub fn symbol<HAST: HyperAST>(stores: &HAST, id: HAST::IdN) -> Symbol {
    let t = stores.resolve_type(&id);
    use hyperast::types::LangRef;
    let id = t.get_lang().ts_symbol(t);
    id.into()
}

pub fn is_visible<HAST: HyperAST>(stores: &HAST, k: <HAST::TS as TypeStore>::Ty) -> bool {
    !k.is_hidden()
}

impl<IdF: Copy> Status for CursorStatus<IdF> {
    type IdF = IdF;

    fn has_later_siblings(&self) -> bool {
        self.has_later_siblings
    }

    fn has_later_named_siblings(&self) -> bool {
        self.has_later_named_siblings
    }

    fn can_have_later_siblings_with_this_field(&self) -> bool {
        self.can_have_later_siblings_with_this_field
    }

    fn field_id(&self) -> Self::IdF {
        self.field_id
    }

    fn has_supertypes(&self) -> bool {
        !self.supertypes.is_empty()
    }

    fn contains_supertype(&self, sym: Symbol) -> bool {
        self.supertypes.contains(&sym)
    }
}

pub(super) trait BitSet {
    fn max_size(&self) -> usize;
    fn set(&mut self, bit: usize, value: bool);
    fn bit(&self, bit: usize) -> bool;
}
impl BitSet for usize {
    fn max_size(&self) -> usize {
        std::mem::size_of::<usize>() * 8
    }
    fn set(&mut self, bit: usize, value: bool) {
        if value {
            *self |= 1 << bit;
        } else {
            *self &= !(1 << bit);
        }
    }
    fn bit(&self, bit: usize) -> bool {
        (*self & (1 << bit)) != 0
    }
}
impl BitSet for u128 {
    fn max_size(&self) -> usize {
        std::mem::size_of::<u128>() * 8
    }
    fn set(&mut self, bit: usize, value: bool) {
        if value {
            *self |= 1 << bit;
        } else {
            *self &= !(1 << bit);
        }
    }
    fn bit(&self, bit: usize) -> bool {
        (*self & (1 << bit)) != 0
    }
}

impl BitSet for Vec<u64> {
    fn max_size(&self) -> usize {
        usize::MAX
    }
    fn set(&mut self, bit: usize, value: bool) {
        while self.len() <= bit / 64 {
            self.push(0);
        }
        if value {
            self[bit / 64] |= 1 << (bit % 64);
        } else {
            self[bit / 64] &= !(1 << (bit % 64));
        }
    }
    fn bit(&self, bit: usize) -> bool {
        self.len() > bit / 64 && (self[bit / 64] & (1 << (bit % 64))) != 0
    }
}
