//! Structs and implementations related to nodes types.
//! Long term the content of this file won't be generated, but factorized.

use std::fmt::Display;

use hyperast::tree_gen::utils_ts::{TsEnableTS, TsType};
use hyperast::types::{AnyType, TypeU16};
use hyperast::types::{HyperType, LangRef, TypeStore, TypeTrait, TypedNodeId};
use hyperast::types::{NodeId, UniformNodeId};

use super::Lang;
use super::TIdN;
use super::TStore;
use super::TType;
use super::{S_T_L, Type};

cfg_if::cfg_if! {if #[cfg(feature = "legion")] {
    use hyperast::types::RoleStore;
    use hyperast::types::LangWrapper;
    use super::Role;

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
            self == Self::String
        }
    }

    impl hyperast::types::ETypeStore for TStore {
        type Ty2 = Type;

        fn intern(ty: Self::Ty2) -> Self::Ty {
            TType::new(ty)
        }
    }

    impl TypeStore for TStore {
        type Ty = TypeU16<Lang>;
    }

    // impl TsEnabledTypeStore for TStore {
    //     fn resolve(t: Self::Ty) -> Type {
    //         t.e()
    //     }
    // }

    impl RoleStore for TStore {
        type IdF = u16;

        type Role = Role;

        fn resolve_field(_lang: LangWrapper<Self::Ty>, field_id: Self::IdF) -> Self::Role {
            let s = crate::language()
                .field_name_for_id(field_id)
                .ok_or_else(|| format!("{}", field_id))
                .unwrap();
            Role::try_from(s).expect(s)
        }

        fn intern_role(_lang: LangWrapper<Self::Ty>, role: Self::Role) -> Self::IdF {
            let field_name = role.to_string();
            crate::language()
                .field_id_for_name(field_name)
                .unwrap()
                .into()
        }
    }
}}

// pub trait TsEnabledTypeStore: hyperast::types::ETypeStore<Ty2 = Type> + Clone + TsEnableTS {
//     fn resolve(t: Self::Ty) -> Type;
// }

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

#[cfg(feature = "impl")]
fn id_for_node_kind(kind: &str, named: bool) -> u16 {
    crate::language().id_for_node_kind(kind, named)
}
#[cfg(not(feature = "impl"))]
fn id_for_node_kind(kind: &str, named: bool) -> u16 {
    unimplemented!("need treesitter grammar")
}

// 356 + directory  + spaces
const COUNT: u16 = 358;

impl Display for Type {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(self.to_str())
    }
}

impl hyperast::types::LLang<TType> for Lang {
    type I = u16;

    type E = Type;

    const TE: &[Self::E] = S_T_L;

    fn as_lang_wrapper() -> hyperast::types::LangWrapper<TType> {
        From::<&'static dyn LangRef<_>>::from(&Lang)
    }
}

impl From<u16> for Type {
    fn from(value: u16) -> Self {
        debug_assert_eq!(Self::from_u16(value), S_T_L[value as usize]);
        S_T_L[value as usize]
    }
}
impl Into<TypeU16<Lang>> for Type {
    fn into(self) -> TypeU16<Lang> {
        TypeU16::new(self)
    }
}

impl Into<u16> for Type {
    fn into(self) -> u16 {
        self as u8 as u16
    }
}

impl<IdN: Clone + Eq + UniformNodeId> TypedNodeId for TIdN<IdN> {
    type Ty = Type;
    type TyErazed = TType;
    fn unerase(ty: Self::TyErazed) -> Self::Ty {
        ty.e()
    }
}

impl Lang {
    pub const NAME: &'static str = "hyperast_gen_ts_python::types::Lang";
    // std::any::type_name::<Lang>() // WAITING for const_type_name feature stability
}

impl LangRef<AnyType> for Lang {
    fn make(&self, _t: u16) -> &'static AnyType {
        panic!()
        // &From::<&'static dyn HyperType>::from(&S_T_L[t as usize])
    }
    fn to_u16(&self, t: AnyType) -> u16 {
        // t as u16
        let t = t.as_any().downcast_ref::<Type>().unwrap();
        *t as u16
    }

    fn name(&self) -> &'static str {
        debug_assert_eq!(std::any::type_name::<Lang>(), Self::NAME);
        Self::NAME
    }

    fn ts_symbol(&self, t: AnyType) -> u16 {
        id_for_node_kind(t.as_static_str(), t.is_named())
    }
}

impl LangRef<Type> for Lang {
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
        debug_assert_eq!(std::any::type_name::<Lang>(), Self::NAME);
        Self::NAME
    }

    fn ts_symbol(&self, t: Type) -> u16 {
        id_for_node_kind(t.as_static_str(), t.is_named())
    }
}

impl LangRef<TType> for Lang {
    fn make(&self, t: u16) -> &'static TType {
        // TODO could make one safe, but not priority
        unsafe { std::mem::transmute(&S_T_L[t as usize]) }
    }
    fn to_u16(&self, t: TType) -> u16 {
        t.e() as u16
    }

    fn name(&self) -> &'static str {
        debug_assert_eq!(std::any::type_name::<Lang>(), Self::NAME);
        Self::NAME
    }

    fn ts_symbol(&self, t: TType) -> u16 {
        id_for_node_kind(t.as_static_str(), t.is_named())
    }
}

impl hyperast::types::Lang<Type> for Lang {
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
        self == &Type::Module
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
        self.is_syntax()
    }

    fn as_shared(&self) -> hyperast::types::Shared {
        use hyperast::types::Shared;
        if self.is_error() {
            return Shared::Error;
        }

        match self {
            Type::ClassDefinition => Shared::TypeDeclaration,
            Type::FunctionDefinition => Shared::TypeDeclaration,
            Type::Comment => Shared::Comment,
            Type::Identifier => Shared::Identifier,
            _ => Shared::Other,
        }
    }

    fn is_error(&self) -> bool {
        self == &Self::ERROR || self == &Self::_ERROR
    }

    fn as_abstract(&self) -> hyperast::types::Abstracts {
        use hyperast::types::Abstract;
        Abstract::Expression.when(self.is_expression())
            | Abstract::Statement.when(self.is_statement())
            | Abstract::Executable.when(self.is_executable_member())
            | Abstract::Declaration.when(self.is_type_declaration())
            | Abstract::Literal.when(self.is_literal())
    }

    fn as_any(&self) -> &dyn std::any::Any {
        self
    }

    fn as_static(&self) -> &'static dyn HyperType {
        let t = <Lang as hyperast::types::Lang<Type>>::to_u16(*self);
        let t = <Lang as hyperast::types::Lang<Type>>::make(t);
        t
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
    type Lang = Lang;

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
