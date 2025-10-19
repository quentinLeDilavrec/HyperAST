use num::ToPrimitive;

use super::NodeIdentifier;
use super::boxing;
use super::compo;
use std::{fmt::Debug, hash::Hash, marker::PhantomData};

use crate::hashed::NodeHashs;
use crate::hashed::SyntaxNodeHashs;
use crate::types::Children;
use crate::{
    hashed::SyntaxNodeHashsKinds,
    nodes::HashSize,
    store::defaults::LabelIdentifier,
    types::{Labeled, NodeId, Typed, TypedNodeId, WithChildren},
};

pub struct HashedNodeRef<'a, Id>(pub(super) &'a boxing::ErasedMap, pub(super) PhantomData<Id>);

impl<Id: TypedNodeId<IdN = NodeIdentifier>> PartialEq for HashedNodeRef<'_, Id> {
    fn eq(&self, other: &Self) -> bool {
        std::ptr::eq(self, other)
    }
}

impl<Id: TypedNodeId<IdN = NodeIdentifier>> Eq for HashedNodeRef<'_, Id> {}

impl<Id: TypedNodeId<IdN = NodeIdentifier>> Hash for HashedNodeRef<'_, Id> {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        crate::types::WithHashs::hash(self, SyntaxNodeHashsKinds::default()).hash(state)
    }
}

impl<Id: 'static + TypedNodeId<IdN = NodeIdentifier>> Debug for HashedNodeRef<'_, Id>
where
    Id::Ty: Debug,
{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut acc = f.debug_struct("HashedNodeRef");
        acc.field("ty", &self.get_type());
        if let Some(label) = self.try_get_label() {
            acc.field("label", label);
        }
        if let Some(children) = &self.children() {
            acc.field("children", children);
        }
        acc.finish()
    }
}

impl<Id: 'static + TypedNodeId<IdN = NodeIdentifier>> crate::types::Typed for HashedNodeRef<'_, Id>
where
    Id::Ty: Copy + Hash + Eq,
{
    type Type = Id::Ty;

    fn get_type(&self) -> Id::Ty {
        *self.0.get::<Id::Ty>().unwrap()
    }
}

impl<Id: TypedNodeId<IdN = NodeIdentifier>> crate::types::WithStats for HashedNodeRef<'_, Id> {
    fn size(&self) -> usize {
        self.0
            .get::<compo::Size>()
            .and_then(|x| x.0.to_usize())
            .unwrap_or(1)
    }

    fn height(&self) -> usize {
        self.0
            .get::<compo::Height>()
            .and_then(|x| x.0.to_usize())
            .unwrap_or(1)
    }

    fn line_count(&self) -> usize {
        self.0
            .get::<compo::LineCount>()
            .and_then(|x| x.0.to_usize())
            .unwrap_or(1)
    }
}
impl<Id: TypedNodeId<IdN = NodeIdentifier>> crate::types::WithSerialization
    for HashedNodeRef<'_, Id>
{
    fn try_bytes_len(&self) -> Option<usize> {
        self.0.get::<compo::BytesLen>().and_then(|x| x.0.to_usize())
    }
}

impl<T> HashedNodeRef<'_, T> {
    pub fn size_no_spaces(&self) -> usize {
        self.0
            .get::<compo::SizeNoSpaces>()
            .and_then(|x| x.0.to_usize())
            .unwrap_or(1)
    }
}

impl<Id: TypedNodeId<IdN = NodeIdentifier>> crate::types::Node for HashedNodeRef<'_, Id> {}

impl<Id: TypedNodeId<IdN = NodeIdentifier>> crate::types::Stored for HashedNodeRef<'_, Id> {
    type TreeId = Id;
}

impl<Id: TypedNodeId<IdN = NodeIdentifier>> HashedNodeRef<'_, Id> {
    pub fn cs(&self) -> Option<crate::types::LendC<'_, Self, u16, NodeIdentifier>> {
        todo!()
        // self.0
        //     .get::<compo::CS<NodeIdentifier>>()
        //     .map(|x| (*x.0).into())
    }
    pub fn no_spaces(&self) -> Option<crate::types::LendC<'_, Self, u16, NodeIdentifier>> {
        todo!()
        // self.0
        //     .get::<compo::NoSpacesCS<NodeIdentifier>>()
        //     .map(|x| &*x.0)
        //     .or_else(|| self.0.get::<compo::CS<NodeIdentifier>>().map(|x| &*x.0))
        //     .map(|x| (*x).into())
    }
}

impl<'a, Id: TypedNodeId<IdN = NodeIdentifier>> crate::types::CLending<'a, u16, Id::IdN>
    for HashedNodeRef<'_, Id>
{
    type Children = crate::types::ChildrenSlice<'a, Id::IdN>;
}

impl<Id: TypedNodeId<IdN = NodeIdentifier>> crate::types::WithChildren for HashedNodeRef<'_, Id> {
    type ChildIdx = u16;
    // type Children<'b>
    //     = MySlice<<Self::TreeId as NodeId>::IdN>
    // where
    //     Self: 'b;

    fn child_count(&self) -> Self::ChildIdx {
        self.cs()
            .map_or(0, |x| {
                let c: u16 = x.child_count();
                c
            })
            .to_u16()
            .expect("too much children")
    }

    fn child(&self, idx: &Self::ChildIdx) -> Option<<Self::TreeId as NodeId>::IdN> {
        let Some(cs) = self.cs() else {
            log::error!("backtrace: {}", std::backtrace::Backtrace::force_capture());
            panic!()
        };
        cs.0.get(idx.to_usize().unwrap()).copied()
    }

    fn child_rev(&self, idx: &Self::ChildIdx) -> Option<<Self::TreeId as NodeId>::IdN> {
        let v = self.cs()?;
        let c: Self::ChildIdx = v.child_count();
        let c = c.checked_sub(idx.checked_add(1)?)?;
        v.get(c).cloned()
    }

    fn children(
        &self,
    ) -> Option<crate::types::LendC<'_, Self, Self::ChildIdx, <Self::TreeId as NodeId>::IdN>> {
        self.cs()
    }
}

impl<Id: TypedNodeId<IdN = NodeIdentifier>> crate::types::WithHashs for HashedNodeRef<'_, Id> {
    type HK = SyntaxNodeHashsKinds;
    type HP = HashSize;

    fn hash(&self, kind: impl std::ops::Deref<Target = Self::HK>) -> Self::HP {
        self.0
            .get::<SyntaxNodeHashs<Self::HP>>()
            .unwrap()
            .hash(&kind)
    }
}

impl<Id> crate::store::nodes::ErasedHolder for HashedNodeRef<'_, Id> {
    fn unerase_ref<T: 'static + Send + Sync>(&self, _tid: std::any::TypeId) -> Option<&T> {
        todo!()
    }
}

impl<Id> crate::store::nodes::PolyglotHolder for HashedNodeRef<'_, Id> {
    fn lang_id(&self) -> crate::store::nodes::LangId {
        unimplemented!()
    }
}

impl<Id: 'static + TypedNodeId<IdN = NodeIdentifier>> crate::types::Tree for HashedNodeRef<'_, Id>
where
    Id::Ty: Copy + Hash + Eq,
{
    fn has_children(&self) -> bool {
        self.cs()
            .map(|x| !crate::types::Childrn::is_empty(&x))
            .unwrap_or(false)
    }

    fn has_label(&self) -> bool {
        self.0.get::<LabelIdentifier>().is_some()
    }
}

impl<Id: TypedNodeId<IdN = NodeIdentifier>> crate::types::Labeled for HashedNodeRef<'_, Id> {
    type Label = LabelIdentifier;

    fn get_label_unchecked(&self) -> &LabelIdentifier {
        self.0
            .get::<LabelIdentifier>()
            .expect("check with self.has_label()")
    }
    fn try_get_label(&self) -> Option<&Self::Label> {
        self.0.get::<LabelIdentifier>()
    }
}

impl<Id: TypedNodeId<IdN = NodeIdentifier>> HashedNodeRef<'_, Id> {
    /// Returns a reference to one of the entity's components.
    pub fn get_component<C: std::marker::Send + std::marker::Sync + 'static>(
        &self,
    ) -> Result<&C, ()> {
        self.0.get::<C>().ok_or(())
    }
}
