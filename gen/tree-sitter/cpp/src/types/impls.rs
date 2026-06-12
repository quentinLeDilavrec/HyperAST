//! Structs and implementations related to nodes types.
//! Long term the content of this file won't be generated, but factorized.

use std::fmt::Display;

use hyperast::tree_gen::TsEnableTS;
use hyperast::tree_gen::TsType;
use hyperast::types::{AnyType, TypeU16};
use hyperast::types::{LangRef, TypeStore, TypeTrait, TypedNodeId};
use hyperast::types::{NodeId, UniformNodeId};

use super::Role;
use super::TIdN;
use super::TStore;
use super::TType;
use super::{Cpp, Lang};
use super::{S_T_L, Type};

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
        const LEN: u16 = S_T_L.len() as u16;
        if (LEN..TStore::LOWEST_RESERVED).contains(&k) {
            return None;
        }
        #[cfg(feature = "impl_intern")]
        debug_assert_eq!(
            crate::language().node_kind_for_id(k).unwrap(),
            Type::from_u16(k).to_str()
        );
        Some(Type::from_u16(k))
    }
}

impl TsType for Type {
    fn spaces() -> Self {
        Self::Spaces
    }

    fn is_repeat(&self) -> bool {
        self.is_repeat()
    }
}

impl TypeStore for TStore {
    type Ty = TypeU16<Cpp>;
}

impl hyperast::types::ETypeStore for TStore {
    type Ty2 = Type;

    fn intern(ty: Self::Ty2) -> Self::Ty {
        TType::new(ty)
    }
}

cfg_if::cfg_if! {if #[cfg(feature = "impl")] {
    use hyperast::types::{LangWrapper, RoleStore};

    impl CppEnabledTypeStore for TStore {
        fn resolve(t: Self::Ty) -> Type {
            t.e()
        }
    }

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

    fn id_for_node_kind(kind: &str, named: bool) -> u16 {
        crate::language().id_for_node_kind(kind, named)
    }

    pub trait CppEnabledTypeStore:
        hyperast::types::ETypeStore<Ty2 = Type> + Clone + hyperast::tree_gen::TsEnableTS
    {
        // fn intern(t: Type) -> Self::Ty;
        fn resolve(t: Self::Ty) -> Type;
    }

} else {
    fn id_for_node_kind(_kind: &str, _named: bool) -> u16 {
        unimplemented!("need treesitter grammar")
    }

    pub trait CppEnabledTypeStore: TypeStore {
        // fn intern(t: Type) -> Self::Ty;
        fn resolve(t: Self::Ty) -> Type;
    }
}}

impl Type {
    pub fn resolve(t: u16) -> Self {
        assert!(t < COUNT);
        unsafe { std::mem::transmute(t) }
    }
}

impl<IdN: Clone + Eq + hyperast::types::UniformNodeId> NodeId for TIdN<IdN> {
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

impl Cpp {
    pub const INST: Cpp = Lang;
    pub const NAME: &'static str = "hyperast_gen_ts_cpp::types::Lang";
    // std::any::type_name::<Lang>() // WAITING for const_type_name feature stability
}

#[cfg(test)]
pub fn as_any(t: &Type) -> AnyType {
    let t = <Cpp as hyperast::types::Lang<Type>>::to_u16(*t);
    let t = <Cpp as hyperast::types::Lang<Type>>::make(t);
    let t: &'static dyn hyperast::types::HyperType = t;
    t.into()
}

impl LangRef<AnyType> for Cpp {
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
        debug_assert_eq!(std::any::type_name::<Cpp>(), Self::NAME);
        Self::NAME
    }

    fn ts_symbol(&self, t: AnyType) -> u16 {
        id_for_node_kind(t.as_static_str(), t.is_named())
    }
}

impl LangRef<Type> for Cpp {
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
            let t = Type::from_u16(t);
            assert_eq!(t, S_T_L[t as usize]);
            &S_T_L[t as usize]
        }
    }
    fn to_u16(&self, t: Type) -> u16 {
        t as u16
    }

    fn name(&self) -> &'static str {
        debug_assert_eq!(std::any::type_name::<Cpp>(), Self::NAME);
        Self::NAME
    }

    fn ts_symbol(&self, t: Type) -> u16 {
        id_for_node_kind(t.to_str(), t.is_named())
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
        use hyperast::types::HyperType;
        id_for_node_kind(t.as_static_str(), t.is_named())
    }
}

impl hyperast::types::Lang<Type> for Cpp {
    const INST: Self = Lang;
    fn make(t: u16) -> &'static Type {
        Lang.make(t)
    }
    fn to_u16(t: Type) -> u16 {
        Lang.to_u16(t)
    }
}

impl hyperast::types::HyperType for Type {
    fn generic_eq(&self, other: &dyn hyperast::types::HyperType) -> bool
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
        self == &Type::TranslationUnit
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
        self.as_shared()
    }

    fn is_error(&self) -> bool {
        self.is_error()
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

    /// ```
    /// # fn main() {
    /// # use hyperast_gen_ts_cpp::types::Type;
    /// # use hyperast::types::HyperType;
    /// let k0 = Type::FunctionDefinition.as_static();
    /// let k1 = Type::FunctionDefinition.as_static();
    /// let k2 = Type::EnumSpecifier.as_static();
    /// assert!(std::ptr::eq(k0,k1));
    /// assert!(!std::ptr::eq(k0,k2));
    /// # }
    /// ```
    fn as_static(&self) -> &'static dyn hyperast::types::HyperType {
        let t = <Cpp as hyperast::types::Lang<Type>>::to_u16(*self);

        (<Cpp as hyperast::types::Lang<Type>>::make(t)) as _
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
        hyperast::types::LangWrapper::from(&Lang as &(dyn LangRef<Self> + 'static))
    }

    fn lang_ref(&self) -> hyperast::types::LangWrapper<AnyType> {
        hyperast::types::LangWrapper::from(&Lang as &(dyn LangRef<AnyType> + 'static))
    }
}
impl TypeTrait for Type {
    type Lang = Cpp;

    fn is_fork(&self) -> bool {
        todo!()
    }

    fn is_literal(&self) -> bool {
        self.is_literal()
    }

    fn is_primitive(&self) -> bool {
        todo!()
    }

    fn is_type_declaration(&self) -> bool {
        todo!()
    }

    fn is_identifier(&self) -> bool {
        self.is_identifier()
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

const COUNT: u16 = 542;
impl Display for Type {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(self.to_str())
    }
}

// impl TryFrom<&str> for Type {
//     type Error = ();

//     fn try_from(value: &str) -> Result<Self, Self::Error> {
//         Type::from_str(value).ok_or(())
//     }
// }
impl hyperast::types::LLang<TType> for Cpp {
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

impl From<Type> for TypeU16<Cpp> {
    fn from(val: Type) -> Self {
        TypeU16::new(val)
    }
}

impl From<Type> for u16 {
    fn from(_val: Type) -> Self {
        todo!()
        // self as u16
    }
}
