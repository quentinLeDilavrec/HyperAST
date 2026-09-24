use std::{borrow::Borrow, marker::PhantomData};

use crate::types::TypeStore;

pub mod labels;
pub mod nodes;
// pub mod ecs; // TODO try a custom ecs ?

pub struct SimpleStores<TS, NS = nodes::DefaultNodeStore, LS = labels::LabelStore> {
    pub label_store: LS,
    pub node_store: NS,
    pub type_store: PhantomData<TS>,
}

#[cfg(feature = "scripting")]
impl<TS> mlua::UserData for SimpleStores<TS> {}

impl<TS, NS, LS> SimpleStores<TS, NS, LS> {
    pub fn change_type_store<TS2>(self) -> SimpleStores<TS2, NS, LS> {
        SimpleStores {
            type_store: PhantomData,
            node_store: self.node_store,
            label_store: self.label_store,
        }
    }
}

/// Enforces explicit conversions between type stores,
/// i.e., implement to declare that Self can be converted to T,
/// e.g. from the git::types::TStore to the Java one, but not the contrary
/// Note: the unsafe keyword is not needed as the assertions ensure that both types are ZSTs, so we can safely transmute
pub trait TyDown<T>: Sized {
    const ASSERT_ZERO_T: () = assert!(std::mem::size_of::<T>() == 0, "T must be a ZST");
    const ASSERT_ZERO_SELF: () = assert!(std::mem::size_of::<Self>() == 0, "Self must be a ZST");
}

impl<TS, NS, LS> SimpleStores<TS, NS, LS> {
    pub fn mut_with_ts<TS2>(&mut self) -> &mut SimpleStores<TS2, NS, LS>
    where
        TS: TyDown<TS2>,
    {
        // SAFETY: TyDown is implemented for TS2 -> TS, thus both are ZSTs, so we can safely transmute
        unsafe { std::mem::transmute(self) }
    }
    pub fn with_ts<TS2>(&self) -> &SimpleStores<TS2, NS, LS>
    where
        TS: TyDown<TS2>,
    {
        // SAFETY: TyDown is implemented for TS2 -> TS, thus both are ZSTs, so we can safely transmute
        unsafe { std::mem::transmute(self) }
    }
}

impl<TS: Default, NS: Default, LS: Default> Default for SimpleStores<TS, NS, LS> {
    fn default() -> Self {
        Self {
            label_store: Default::default(),
            type_store: Default::default(),
            node_store: Default::default(),
        }
    }
}

impl<TS: Copy, NS: Copy, LS: Copy> Copy for SimpleStores<TS, NS, LS> {}
impl<TS: Clone, NS: Clone, LS: Clone> Clone for SimpleStores<TS, NS, LS> {
    fn clone(&self) -> Self {
        Self {
            label_store: self.label_store.clone(),
            node_store: self.node_store.clone(),
            type_store: self.type_store,
        }
    }
}

impl<TS, NS, LS> crate::types::RoleStore for SimpleStores<TS, NS, LS>
where
    TS: crate::types::RoleStore,
{
    type IdF = TS::IdF;

    type Role = TS::Role;

    fn resolve_field(
        lang: impl crate::types::LangRef<Self::Ty>,
        field_id: Self::IdF,
    ) -> Self::Role {
        TS::resolve_field(lang, field_id)
    }
    fn intern_role(lang: impl crate::types::LangRef<Self::Ty>, role: Self::Role) -> Self::IdF {
        TS::intern_role(lang, role)
    }
}

impl<IdN, TS, NS, LS> crate::types::NodeStoreLean<IdN> for SimpleStores<TS, NS, LS>
where
    NS::R: crate::types::Tree<TreeId = IdN>,
    IdN: crate::types::UniformNodeId,
    NS: crate::types::NodeStoreLean<IdN>,
{
    type R = NS::R;

    fn resolve(&self, id: &IdN) -> Self::R {
        self.node_store.resolve(id)
    }
}

impl<TS, NS, LS> crate::types::LabelStore<str> for SimpleStores<TS, NS, LS>
where
    LS: crate::types::LabelStore<str>,
{
    type I = LS::I;

    fn get_or_insert<U: Borrow<str>>(&mut self, node: U) -> Self::I {
        self.label_store.get_or_insert(node)
    }

    fn get<U: Borrow<str>>(&self, node: U) -> Option<Self::I> {
        self.label_store.get(node)
    }

    fn resolve(&self, id: &Self::I) -> &str {
        self.label_store.resolve(id)
    }
}

impl<TS, NS, LS> crate::types::TypeStore for SimpleStores<TS, NS, LS>
where
    TS::Ty: 'static + std::hash::Hash,
    TS: TypeStore,
{
    type Ty = TS::Ty;
}

pub mod defaults {
    pub type LabelIdentifier = super::labels::DefaultLabelIdentifier;
    pub type LabelValue = super::labels::DefaultLabelValue;
    pub type NodeIdentifier = super::nodes::DefaultNodeIdentifier;
}
