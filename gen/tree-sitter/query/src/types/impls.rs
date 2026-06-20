//! Structs and implementations related to nodes types.
//! Long term the content of this file won't be generated, but factorized.

use std::fmt::Display;

use hyperast::tree_gen::{TsEnableTS, TsType};
use hyperast::types::{AnyType, TypeU16};
use hyperast::types::{HyperType, LangRef, TypeStore, TypeTrait, TypedNodeId};
use hyperast::types::{NodeId, UniformNodeId};

use super::TIdN;
use super::TStore;
use super::TType;
use super::{Lang, TsQuery};
use super::{S_T_L, Type};

impl hyperast::types::ETypeStore for TStore {
    type Ty2 = Type;

    fn intern(ty: Self::Ty2) -> Self::Ty {
        TType::new(ty)
    }
}

impl TsEnableTS for TStore {
    fn obtain_type<'a, N: hyperast::tree_gen::parser::NodeWithU16TypeId>(
        n: &N,
    ) -> <Self as hyperast::types::ETypeStore>::Ty2 {
        let k = n.kind_id();
        Type::from_u16(k)
    }

    fn try_obtain_type<N: hyperast::tree_gen::parser::NodeWithU16TypeId>(
        n: &N,
    ) -> Option<Self::Ty2> {
        let k = n.kind_id();
        static LEN: u16 = S_T_L.len() as u16;
        if LEN <= k && k < TStore::LOWEST_RESERVED {
            return None;
        }
        Some(Type::from_u16(k))
    }
}

impl TsType for Type {
    fn spaces() -> Self {
        Self::Spaces
    }

    fn error() -> Self {
        Self::ERROR
    }

    fn is_repeat(&self) -> bool {
        self.is_repeat()
    }

    fn is_leaf(self) -> bool {
        false
    }
}

impl TypeStore for TStore {
    type Ty = TypeU16<TsQuery>;
}

cfg_if::cfg_if! {if #[cfg(feature = "legion")] {
    use hyperast::store::defaults::NodeIdentifier;
    use hyperast::store::nodes::legion::HashedNodeRef;
    use hyperast::types::LangWrapper;
    use hyperast::types::RoleStore;
    use super::Role;

    impl RoleStore for TStore {
        type IdF = u16;

        type Role = Role;

        fn resolve_field(_lang: LangWrapper<Self::Ty>, field_id: Self::IdF) -> Self::Role {
            let s = tree_sitter_query::language()
                .field_name_for_id(field_id)
                .ok_or_else(|| format!("{}", field_id))
                .unwrap();
            Role::try_from(s).expect(s)
        }

        fn intern_role(_lang: LangWrapper<Self::Ty>, role: Self::Role) -> Self::IdF {
            let field_name = role.to_string();
            tree_sitter_query::language()
                .field_id_for_name(field_name)
                .unwrap()
                .into()
        }
    }

    impl TsQueryEnabledTypeStore<HashedNodeRef<'_, NodeIdentifier>> for TStore {
        fn resolve(t: Self::Ty) -> Type {
            t.e()
        }
    }
}}

#[cfg(feature = "impl")]
fn id_for_node_kind(kind: &str, named: bool) -> u16 {
    tree_sitter_query::language().id_for_node_kind(kind, named)
}
#[cfg(not(feature = "impl"))]
fn id_for_node_kind(_kind: &str, _named: bool) -> u16 {
    unimplemented!("need treesitter grammar")
}

pub trait TsQueryEnabledTypeStore<T>:
    hyperast::types::ETypeStore<Ty2 = Type> + Clone + TsEnableTS
{
    fn resolve(t: Self::Ty) -> Type;
}

impl Type {
    pub fn resolve(t: u16) -> Self {
        assert!(t < COUNT);
        unsafe { std::mem::transmute(t) }
    }
}

impl<IdN: Clone + Eq + UniformNodeId> NodeId for TIdN<IdN> {
    type IdN = IdN;

    fn as_id(&self) -> &Self::IdN {
        &self.0
    }

    unsafe fn from_id(id: Self::IdN) -> Self {
        Self(id)
    }

    unsafe fn from_ref_id(_id: &Self::IdN) -> &Self {
        todo!()
    }
}

impl<IdN: Clone + Eq + UniformNodeId> TypedNodeId for TIdN<IdN> {
    type Ty = Type;
    type TyErazed = TType;
    fn unerase(ty: Self::TyErazed) -> Self::Ty {
        ty.e()
    }
}

impl hyperast::store::TyDown<TStore> for TStore {}

impl Lang {
    pub const NAME: &'static str = "hyperast_gen_ts_tsquery::types::Lang";
    // std::any::type_name::<Lang>() // WAITING for const_type_name feature stability
}

impl LangRef<AnyType> for TsQuery {
    fn make(&self, t: u16) -> &'static AnyType {
        panic!("{}", t)
        // &From::<&'static dyn HyperType>::from(&S_T_L[t as usize])
    }
    fn to_u16(&self, t: AnyType) -> u16 {
        // t as u16
        let t = t.as_any().downcast_ref::<Type>().unwrap();
        *t as u16
    }

    fn name(&self) -> &'static str {
        debug_assert_eq!(std::any::type_name::<TsQuery>(), Self::NAME);
        Self::NAME
    }

    fn ts_symbol(&self, t: AnyType) -> u16 {
        id_for_node_kind(t.as_static_str(), t.is_named())
    }
}

impl LangRef<Type> for TsQuery {
    fn make(&self, t: u16) -> &'static Type {
        if t == TStore::ERROR {
            &Type::ERROR
        } else if t == TStore::_ERROR {
            &Type::_ERROR
        } else if t == TStore::SPACES {
            &Type::Spaces
        } else if t == TStore::DIRECTORY {
            &Type::Directory
        } else {
            &S_T_L[t as usize]
        }
    }
    fn to_u16(&self, t: Type) -> u16 {
        t as u16
    }

    fn name(&self) -> &'static str {
        debug_assert_eq!(std::any::type_name::<TsQuery>(), Self::NAME);
        Self::NAME
    }

    fn ts_symbol(&self, t: Type) -> u16 {
        id_for_node_kind(t.as_static_str(), t.is_named())
    }
}

impl LangRef<TType> for TsQuery {
    fn make(&self, t: u16) -> &'static TType {
        // TODO could make one safe, but not priority
        unsafe { std::mem::transmute(&S_T_L[t as usize]) }
    }
    fn to_u16(&self, t: TType) -> u16 {
        t.e() as u16
    }

    fn name(&self) -> &'static str {
        debug_assert_eq!(std::any::type_name::<TsQuery>(), Self::NAME);
        Self::NAME
    }

    fn ts_symbol(&self, t: TType) -> u16 {
        id_for_node_kind(t.as_static_str(), t.is_named())
    }
}

impl hyperast::types::Lang<Type> for TsQuery {
    const INST: Self = Lang;
    fn make(t: u16) -> &'static Type {
        Lang.make(t)
    }
    fn to_u16(t: Type) -> u16 {
        Lang.to_u16(t)
    }
}

impl HyperType for Type {
    fn generic_eq(&self, other: &dyn HyperType) -> bool
    where
        Self: 'static + PartialEq + Sized,
    {
        // Do a type-safe casting. If the types are different,
        // return false, otherwise test the values for equality.
        other
            .as_any()
            .downcast_ref::<Self>()
            .map_or(false, |a| self == a)
    }
    fn is_directory(&self) -> bool {
        self == &Type::Directory
    }

    fn is_file(&self) -> bool {
        self == &Type::Program
    }

    fn is_spaces(&self) -> bool {
        self == &Type::Spaces
        // setting TS0 as space is causing an issue with global_pos_with_spaces
        // and TS0 is end list of tokens, so maybe other issues.
        // Actual fix is to skip TS0 in skipable_pre in the generator,
        // thus TSO should not appear anymore in generated ast.
        // || self == &Type::TS0
    }

    fn is_syntax(&self) -> bool {
        todo!()
    }

    fn as_shared(&self) -> hyperast::types::Shared {
        use hyperast::types::Shared;
        if self.is_error() {
            return Shared::Error;
        }

        match self {
            Type::Comment => Shared::Comment,
            Type::Identifier => Shared::Identifier,
            _ => Shared::Other,
        }
    }

    fn is_error(&self) -> bool {
        self == &Self::ERROR || self == &Self::_ERROR
    }

    fn as_abstract(&self) -> hyperast::types::Abstracts {
        hyperast::types::Abstracts::empty()
    }

    fn as_any(&self) -> &dyn std::any::Any {
        self
    }

    fn as_static(&self) -> &'static dyn HyperType {
        let t = <TsQuery as hyperast::types::Lang<Type>>::to_u16(*self);

        (<TsQuery as hyperast::types::Lang<Type>>::make(t)) as _
    }

    fn as_static_str(&self) -> &'static str {
        self.to_str()
    }

    fn is_hidden(&self) -> bool {
        self.is_hidden()
    }

    fn is_supertype(&self) -> bool {
        self.is_supertype()
    }

    fn is_named(&self) -> bool {
        self.is_named()
    }

    fn get_lang(&self) -> hyperast::types::LangWrapper<Self>
    where
        Self: Sized,
    {
        From::<&'static dyn LangRef<Self>>::from(&Lang)
    }
    fn lang_ref(&self) -> hyperast::types::LangWrapper<AnyType> {
        hyperast::types::LangWrapper::from(&Lang as &(dyn LangRef<AnyType> + 'static))
    }
}

impl TypeTrait for Type {
    type Lang = TsQuery;

    fn is_fork(&self) -> bool {
        todo!()
    }

    fn is_literal(&self) -> bool {
        todo!()
    }

    fn is_primitive(&self) -> bool {
        todo!()
    }

    fn is_type_declaration(&self) -> bool {
        todo!()
    }

    fn is_identifier(&self) -> bool {
        todo!()
    }

    fn is_instance_ref(&self) -> bool {
        todo!()
    }

    fn is_type_body(&self) -> bool {
        todo!()
    }

    fn is_value_member(&self) -> bool {
        todo!()
    }

    fn is_executable_member(&self) -> bool {
        todo!()
    }

    fn is_statement(&self) -> bool {
        todo!()
    }

    fn is_declarative_statement(&self) -> bool {
        todo!()
    }

    fn is_structural_statement(&self) -> bool {
        todo!()
    }

    fn is_block_related(&self) -> bool {
        todo!()
    }

    fn is_simple_statement(&self) -> bool {
        todo!()
    }

    fn is_local_declare(&self) -> bool {
        todo!()
    }

    fn is_parameter(&self) -> bool {
        todo!()
    }

    fn is_parameter_list(&self) -> bool {
        todo!()
    }

    fn is_argument_list(&self) -> bool {
        todo!()
    }

    fn is_expression(&self) -> bool {
        todo!()
    }

    fn is_comment(&self) -> bool {
        todo!()
    }
}

// 356 + directory  + spaces
const COUNT: u16 = 46 + 1 + 1;

impl Display for Type {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(self.to_str())
    }
}

impl TryFrom<&str> for Type {
    type Error = String;

    fn try_from(value: &str) -> Result<Self, Self::Error> {
        Type::from_str(value).ok_or_else(|| value.to_owned())
    }
}

impl hyperast::types::LLang<hyperast::types::TypeU16<Self>> for TsQuery {
    type I = u16;

    type E = Type;

    const TE: &[Self::E] = S_T_L;

    fn as_lang_wrapper() -> hyperast::types::LangWrapper<hyperast::types::TypeU16<Self>> {
        From::<&'static dyn LangRef<_>>::from(&Lang)
    }
}

impl From<u16> for Type {
    fn from(value: u16) -> Self {
        debug_assert_eq!(Self::from_u16(value), S_T_L[value as usize]);
        S_T_L[value as usize]
    }
}

impl From<Type> for TypeU16<TsQuery> {
    fn from(val: Type) -> Self {
        TypeU16::new(val)
    }
}

impl From<Type> for u16 {
    fn from(val: Type) -> Self {
        val as u16
    }
}

#[test]
fn test_tslanguage_and_type_identity() {
    let l = crate::language();
    assert_eq!(l.node_kind_count(), S_T_L.len());
    for id in 0..l.node_kind_count() {
        let kind = l.node_kind_for_id(id as u16).unwrap();
        let ty = Type::from_u16(id as u16);
        assert_eq!(ty.to_str(), kind);
    }
}
