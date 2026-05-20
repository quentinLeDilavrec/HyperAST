use std::cmp::PartialEq;
use std::fmt::Debug;
use std::hash::Hash;
use std::marker::{PhantomData, Send, Sync};
use std::ops::Deref;

use legion::storage::{Archetype, Component};
use legion::world::{ComponentError, EntityLocation};
use num::ToPrimitive;

use crate::filter::{BF, Bloom, BloomResult, BloomSize};
use crate::hashed::{NodeHashs, SyntaxNodeHashs, SyntaxNodeHashsKinds};
use crate::impact::serialize::{CachedHasher, Keyed, MySerialize};
use crate::nodes::{CompressedNode, HashSize, RefContainer};
use crate::store::defaults::LabelIdentifier;
use crate::store::nodes::compo::{self, CS, NoSpacesCS};
use crate::types::NodeId;
use crate::types::TypedNodeId;
use crate::types::{AnyType, Children, HyperType, TypeTrait, Typed};
use crate::types::{WithChildren, WithMetaData};

// TODO refactor alias into a transparent struct
pub type NodeIdentifier = legion::Entity;
pub type EntryRef<'a> = legion::world::EntryRef<'a>;
#[derive(ref_cast::RefCast)]
#[repr(transparent)]
pub struct HashedNodeRef<'a, T = NodeIdentifier>(pub(super) EntryRef<'a>, PhantomData<T>);

impl<'a, T> HashedNodeRef<'a, T> {
    #[doc(hidden)]
    pub fn cast_type<U: NodeId>(self) -> HashedNodeRef<'a, U>
    where
        T: NodeId<IdN = U::IdN>,
    {
        HashedNodeRef(self.0, PhantomData)
    }
    pub(super) fn new(e: EntryRef<'a>) -> Self {
        Self(e, PhantomData)
    }
}
impl<'a, T> From<&'a EntryRef<'a>> for &'a HashedNodeRef<'a, T> {
    fn from(value: &'a EntryRef<'a>) -> Self {
        use ref_cast::RefCast;
        // NOTE it does compile time layout assertions
        HashedNodeRef::ref_cast(value)
    }
}

impl NodeId for NodeIdentifier {
    type IdN = Self;
    fn as_id(&self) -> &Self::IdN {
        self
    }
    unsafe fn from_id(id: Self::IdN) -> Self {
        id
    }

    unsafe fn from_ref_id(id: &Self::IdN) -> &Self {
        id
    }
}

impl TypedNodeId for NodeIdentifier {
    type Ty = crate::types::AnyType;
    type TyErazed = crate::types::AnyType;

    fn unerase(ty: Self::TyErazed) -> Self::Ty {
        ty
    }
}

// * hashed node reference impl

impl<Id: TypedNodeId<IdN = NodeIdentifier>> PartialEq for HashedNodeRef<'_, Id> {
    fn eq(&self, other: &Self) -> bool {
        self.0.location().archetype() == other.0.location().archetype()
            && self.0.location().component() == other.0.location().component()
    }
}

impl<Id: TypedNodeId<IdN = NodeIdentifier>> Eq for HashedNodeRef<'_, Id> {}

impl<Id: TypedNodeId<IdN = NodeIdentifier>> Hash for HashedNodeRef<'_, Id> {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        crate::types::WithHashs::hash(self, SyntaxNodeHashsKinds::default()).hash(state)
    }
}

impl<Id: TypedNodeId<IdN = NodeIdentifier>> Debug for HashedNodeRef<'_, Id> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_tuple("HashedNodeRef")
            .field(&self.0.location())
            .finish()
    }
}

impl<'a, Id> Deref for HashedNodeRef<'a, Id> {
    type Target = EntryRef<'a>;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl<Id: TypedNodeId<IdN = NodeIdentifier>> HashedNodeRef<'_, Id> {
    // TODO when relativisation is applied, caller of this method should provide the size of the paren ident
    pub fn get_bytes_len(&self, _p_indent_len: u32) -> u32
    where
        Id::Ty: 'static + TypeTrait + Send + Sync + Debug,
    {
        self.0.get_component::<compo::BytesLen>().unwrap().0
    }
}

impl<Id: 'static + TypedNodeId<IdN = NodeIdentifier>> HashedNodeRef<'_, Id> {
    // TODO when relativisation is applied, caller of this method should provide the size of the paren ident
    pub fn try_get_bytes_len(&self, _p_indent_len: u32) -> Option<u32>
    where
        Id::Ty: HyperType + Copy + Send + Sync,
    {
        if self.get_type().is_spaces() {
            self.0.get_component::<compo::BytesLen>().map(|x| x.0).ok()
        } else {
            self.0.get_component::<compo::BytesLen>().map(|x| x.0).ok()
        }
    }

    pub fn is_directory(&self) -> bool
    where
        Id: HyperType + Copy + Send + Sync,
    {
        self.get_type().is_directory()
    }
}

impl<T, C: Component> WithMetaData<C> for HashedNodeRef<'_, T> {
    fn get_metadata(&self) -> Option<&C> {
        self.0.get_component::<C>().ok()
    }
}

impl<'a, T> HashedNodeRef<'a, T> {
    /// Returns the entity's archetype.
    pub fn archetype(&self) -> &Archetype {
        self.0.archetype()
    }

    /// Returns the entity's location.
    pub fn location(&self) -> EntityLocation {
        self.0.location()
    }

    /// Returns a reference to one of the entity's components.
    pub fn into_component<C: Component>(self) -> Result<&'a C, ComponentError> {
        self.0.into_component::<C>()
    }

    /// Returns a mutable reference to one of the entity's components.
    ///
    /// # Safety
    /// This function bypasses static borrow checking. The caller must ensure that the component reference
    /// will not be mutably aliased.
    pub unsafe fn into_component_unchecked<C: Component>(
        self,
    ) -> Result<&'a mut C, ComponentError> {
        unsafe { self.0.into_component_unchecked::<C>() }
    }

    /// Returns a reference to one of the entity's components.
    pub fn get_component<C: Component>(&self) -> Result<&C, ComponentError> {
        self.0.get_component::<C>()
    }
}

impl<'a, T: crate::types::NodeId<IdN = NodeIdentifier>> HashedNodeRef<'a, T> {
    pub fn get_child_by_name(
        &self,
        name: &<HashedNodeRef<'a, T> as crate::types::Labeled>::Label,
    ) -> Option<NodeIdentifier> {
        let labels = self
            .0
            .get_component::<CS<<HashedNodeRef<'a, T> as crate::types::Labeled>::Label>>()
            .ok()?;
        let idx = labels.0.iter().position(|x| x == name);
        idx.map(|idx| self.child(&idx.to_u16().unwrap()).unwrap())
    }

    pub fn get_child_idx_by_name(
        &self,
        name: &<HashedNodeRef<'a, T> as crate::types::Labeled>::Label,
    ) -> Option<<HashedNodeRef<'a, T> as crate::types::WithChildren>::ChildIdx> {
        let labels = self
            .0
            .get_component::<CS<<HashedNodeRef<'a, T> as crate::types::Labeled>::Label>>()
            .ok()?;
        labels
            .0
            .iter()
            .position(|x| x == name)
            .map(|x| x.to_u16().unwrap())
    }

    pub fn try_get_children_name(
        &self,
    ) -> Option<&[<HashedNodeRef<'a, T> as crate::types::Labeled>::Label]> {
        self.0
            .get_component::<CS<<HashedNodeRef<'a, T> as crate::types::Labeled>::Label>>()
            .ok()
            .map(|x| &*x.0)
    }
}

impl<Id: TypedNodeId<IdN = NodeIdentifier>> HashedNodeRef<'_, Id>
where
    Id::Ty: 'static + Sync + Send + TypeTrait,
{
    pub fn into_compressed_node(
        &self,
    ) -> Result<CompressedNode<legion::Entity, LabelIdentifier, Id::Ty>, ComponentError> {
        let kind = self.0.get_component::<Id::Ty>()?;
        if kind.is_spaces() {
            let spaces = self.0.get_component::<LabelIdentifier>().unwrap();
            return Ok(CompressedNode::Spaces(*spaces));
        }
        let a = self.0.get_component::<LabelIdentifier>();
        let label: Option<LabelIdentifier> = a.ok().copied();
        let children = self.children().map(|x| x.collect());
        Ok(CompressedNode::new(
            *kind,
            label,
            children.unwrap_or_default(),
        ))
    }
}

impl<'a, T> AsRef<HashedNodeRef<'a, T>> for HashedNodeRef<'a, T> {
    fn as_ref(&self) -> &HashedNodeRef<'a, T> {
        self
    }
}

impl<Id: 'static + TypedNodeId<IdN = NodeIdentifier>> crate::types::Typed
    for HashedNodeRef<'_, Id>
{
    type Type = Id::Ty;

    fn get_type(&self) -> Id::Ty
    where
        Id::Ty: Copy + Send + Sync,
    {
        match self.0.get_component::<Id::TyErazed>() {
            Ok(t) => Id::unerase(t.clone()),
            e => Id::unerase(e.unwrap().clone()),
        }
    }
    fn try_get_type(&self) -> Option<Self::Type> {
        self.0.get_component::<Id::Ty>().ok().copied()
    }
}
impl<Id: 'static + TypedNodeId<IdN = NodeIdentifier>> crate::types::Typed
    for &HashedNodeRef<'_, Id>
{
    type Type = AnyType;

    fn get_type(&self) -> AnyType {
        match self.0.get_component::<Id::Ty>() {
            Ok(t) => {
                let t: &'static dyn HyperType = t.as_static();
                t.into()
            }
            Err(e @ ComponentError::NotFound { .. }) => {
                todo!("{:?}", e)
            }
            e => {
                todo!("{:?}", e)
            }
        }
    }
}

impl<T> crate::types::WithStats for HashedNodeRef<'_, T> {
    fn size(&self) -> usize {
        self.0
            .get_component::<compo::Size>()
            .ok()
            .and_then(|x| x.0.to_usize())
            .unwrap_or(1)
    }

    fn height(&self) -> usize {
        self.0
            .get_component::<compo::Height>()
            .ok()
            .and_then(|x| x.0.to_usize())
            .unwrap_or(1)
    }

    fn line_count(&self) -> usize {
        self.0
            .get_component::<compo::LineCount>()
            .ok()
            .and_then(|x| x.0.to_usize())
            .unwrap_or(0)
    }
}

impl<T> HashedNodeRef<'_, T> {
    pub fn size_no_spaces(&self) -> usize {
        self.0
            .get_component::<compo::SizeNoSpaces>()
            .ok()
            .and_then(|x| x.0.to_usize())
            .unwrap_or(1)
    }
}

impl<T> crate::types::WithSerialization for HashedNodeRef<'_, T> {
    fn try_bytes_len(&self) -> Option<usize> {
        self.0
            .get_component::<compo::BytesLen>()
            .ok()
            .map(|x| x.0.to_usize().unwrap())
    }
}

impl<T> crate::types::Labeled for HashedNodeRef<'_, T> {
    type Label = LabelIdentifier;

    fn get_label_unchecked(&self) -> &LabelIdentifier {
        self.0
            .get_component::<LabelIdentifier>()
            .expect("check with self.has_label()")
    }

    fn try_get_label(&self) -> Option<&Self::Label> {
        self.0.get_component::<LabelIdentifier>().ok()
    }
}

impl<T> crate::types::Node for HashedNodeRef<'_, T> {}

impl<T: crate::types::NodeId> crate::types::Stored for HashedNodeRef<'_, T> {
    type TreeId = T;
}

impl<T: crate::types::NodeId> HashedNodeRef<'_, T>
where
    <T as crate::types::NodeId>::IdN: Send + Sync,
{
    pub fn cs(&self) -> Result<crate::types::LendC<'_, Self, u16, T::IdN>, ComponentError> {
        macro_rules! C {
            (* $t:ty) => {
                self.0.get_component::<$t>().map(|x| (*x.0).into())
            };
            ($t:ty) => {
                self.0.get_component::<$t>().map(|x| (&x.0).into())
            };
        }
        C!(* CS<T::IdN>)
            .or_else(|_| C!(compo::CS0<T::IdN, 1>))
            .or_else(|_| C!(compo::CS0<T::IdN, 2>))
    }

    pub fn no_spaces(&self) -> Result<crate::types::LendC<'_, Self, u16, T::IdN>, ComponentError> {
        macro_rules! C {
            (* $t:ty) => {
                self.0.get_component::<$t>().map(|x| (*x.0).into())
            };
            ($t:ty) => {
                self.0.get_component::<$t>().map(|x| (&x.0).into())
            };
        }
        C!(*NoSpacesCS<T::IdN>)
            .or_else(|_| C!(compo::NoSpacesCS0<T::IdN, 1>))
            .or_else(|_| C!(compo::NoSpacesCS0<T::IdN, 2>))
            .or_else(|_| self.cs())
    }
}

impl<'a, T: crate::types::NodeId> crate::types::CLending<'a, u16, T::IdN> for HashedNodeRef<'_, T> {
    type Children = crate::types::ChildrenSlice<'a, T::IdN>;
}

impl<T: crate::types::NodeId> crate::types::WithChildren for HashedNodeRef<'_, T>
where
    <T as crate::types::NodeId>::IdN: Send + Sync + Copy,
{
    type ChildIdx = u16;

    fn child_count(&self) -> u16 {
        self.cs()
            .map_or(0, |x| {
                let c: u16 = x.child_count();
                c
            })
            .to_u16()
            .expect("too much children")
    }

    fn child(&self, idx: &Self::ChildIdx) -> Option<T::IdN> {
        let cs = self.cs().ok()?;
        cs.0.get(idx.to_usize().unwrap()).copied()
    }

    fn child_rev(&self, idx: &Self::ChildIdx) -> Option<T::IdN> {
        let v = self.cs().ok()?;
        let c: Self::ChildIdx = v.child_count();
        let c = c.checked_sub(idx.checked_add(1)?)?;
        v.get(c).cloned()
    }

    fn children(
        &self,
    ) -> Option<crate::types::LendC<'_, Self, Self::ChildIdx, <Self::TreeId as NodeId>::IdN>> {
        self.cs().ok()
    }
}

impl<T: crate::types::NodeId> crate::types::WithRoles for HashedNodeRef<'_, T>
where
    <T as crate::types::NodeId>::IdN: Send + Sync + Copy,
{
    /// Actually `at` works as a structural offset when hidden children can hold fields.
    /// NOTE cannot easily go in children to make it a proper offset then.
    fn role_at<Role: 'static + Copy + Sync + Send>(&self, at: Self::ChildIdx) -> Option<Role> {
        let r = &self.0.get_component::<compo::Roles<Role>>().ok()?.0;
        let ro = self.0.get_component::<compo::RoleOffsets>().ok()?;
        for (i, &ro) in ro.0.as_ref().iter().enumerate() {
            if ro as u16 > at {
                return None;
            } else if ro as u16 == at {
                return Some(r[i]);
            }
        }
        None
    }
    fn role_at_and_has_later<Role: 'static + Copy + Sync + Send + PartialEq>(
        &self,
        at: Self::ChildIdx,
    ) -> Option<(Role, bool)> {
        let r = &self.0.get_component::<compo::Roles<Role>>().ok()?.0;
        let ro = self.0.get_component::<compo::RoleOffsets>().ok()?;
        let mut i = 0;
        for &ro in ro.0.as_ref() {
            if ro as u16 > at {
                return None;
            } else if ro as u16 == at {
                let res = r[i];
                i += 1;
                while i < r.len() {
                    if res == r[i] {
                        return Some((res, true));
                    }
                    i += 1;
                }
                return Some((res, false));
            }
            i += 1;
        }
        None
    }
}

impl<T> crate::types::WithPrecompQueries for HashedNodeRef<'_, T> {
    fn wont_match_given_precomputed_queries(&self, needed: u16) -> bool {
        if needed == num::zero() {
            return false;
        }
        let Ok(v) = self.get_component::<compo::Precomp<u16>>() else {
            return self.get_component::<compo::PrecompFlag>().is_ok();
        };
        v.0 & needed != needed
    }
}

impl<T> crate::types::WithHashs for HashedNodeRef<'_, T> {
    type HK = SyntaxNodeHashsKinds;
    type HP = HashSize;

    fn hash(&self, kind: impl std::ops::Deref<Target = Self::HK>) -> Self::HP {
        self.0
            .get_component::<SyntaxNodeHashs<Self::HP>>()
            .unwrap()
            .hash(&kind)
    }
}

impl<Id> crate::store::nodes::ErasedHolder for HashedNodeRef<'_, Id> {
    fn unerase_ref<T: 'static + Send + Sync>(&self, tid: std::any::TypeId) -> Option<&T> {
        if tid == std::any::TypeId::of::<T>() {
            self.get_component().ok()
        } else {
            None
        }
    }
}

impl<Id> crate::store::nodes::PolyglotHolder for HashedNodeRef<'_, Id> {
    fn lang_id(&self) -> crate::store::nodes::LangId {
        let component_types = self.0.archetype().layout().component_types();
        let lang = component_types[0];
        // TODO add debug assert verifying that it is a valid language identifier
        crate::store::nodes::LangId {
            id: lang.type_id(),
            #[cfg(debug_assertions)]
            name: lang.name,
        }
    }
}

impl<Id> HashedNodeRef<'_, Id> {
    pub fn layout(&self) -> &legion::storage::EntityLayout {
        self.0.archetype().layout().as_ref()
    }
}

impl<Id: 'static + TypedNodeId<IdN = NodeIdentifier>> crate::types::Tree for HashedNodeRef<'_, Id> {
    fn has_children(&self) -> bool {
        self.cs()
            .map(|x| !crate::types::Childrn::is_empty(&x))
            .unwrap_or(false)
    }

    fn has_label(&self) -> bool {
        self.0.get_component::<LabelIdentifier>().is_ok()
    }
}

impl<T> HashedNodeRef<'_, T> {}

impl<T> RefContainer for HashedNodeRef<'_, T> {
    type Result = BloomResult;

    fn check<U: MySerialize + Keyed<usize>>(&self, rf: U) -> Self::Result {
        use crate::filter::BF as _;

        let Ok(e) = self.0.get_component::<BloomSize>() else {
            return BloomResult::MaybeContain;
        };
        macro_rules! check {
        ( $($t:ty),* ) => {
            match *e {
                BloomSize::Much => {
                    log::trace!("[Too Much]");
                    BloomResult::MaybeContain
                },
                BloomSize::None => BloomResult::DoNotContain,
                $( <$t>::SIZE => {
                    let x = CachedHasher::<usize,<$t as BF<[u8]>>::S, <$t as BF<[u8]>>::H>::once(rf);
                    let x = x.into_iter().map(|x|<$t>::check_raw(self.0.get_component::<$t>().unwrap(), x));

                    for x in x {
                        if let BloomResult::MaybeContain = x {
                            return BloomResult::MaybeContain
                        }
                    }
                    BloomResult::DoNotContain
                }),*
            }
        };
    }
        check![
            Bloom<&'static [u8], u16>,
            Bloom<&'static [u8], u32>,
            Bloom<&'static [u8], u64>,
            Bloom<&'static [u8], [u64; 2]>,
            Bloom<&'static [u8], [u64; 4]>,
            Bloom<&'static [u8], [u64; 8]>,
            Bloom<&'static [u8], [u64; 16]>,
            Bloom<&'static [u8], [u64; 32]>,
            Bloom<&'static [u8], [u64; 64]>
        ]
    }
}
