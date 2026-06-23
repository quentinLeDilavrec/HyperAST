//! Structs and implementations related to nodes types.
//! Long term the content of this file won't be generated, but factorized.

use std::fmt::Display;

use hyperast::tree_gen::TsEnableTS;
use hyperast::types::{AnyType, TypeU16};
use hyperast::types::{HyperType, LangRef, TypeStore, TypeTrait, TypedNodeId};
use hyperast::types::{NodeId, UniformNodeId};

use super::Role;
use super::TIdN;
use super::TStore;
use super::TType;
use super::{C, Lang};
use super::{S_T_L, Type};

cfg_if::cfg_if! {if #[cfg(feature = "impl")] {
    use hyperast::tree_gen::utils_ts::TsType;

    use hyperast::types::{LangWrapper, RoleStore};

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
        type Ty = TypeU16<C>;
    }
    impl CEnabledTypeStore for TStore {
        fn resolve(t: Self::Ty) -> Type {
            t.e()
        }
    }

    impl hyperast::types::ETypeStore for TStore {
        type Ty2 = Type;

        fn intern(ty: Self::Ty2) -> Self::Ty {
            TType::new(ty)
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

    pub trait CEnabledTypeStore:
        hyperast::types::ETypeStore<Ty2 = Type> + Clone + hyperast::tree_gen::utils_ts::TsEnableTS
    {
        // fn intern(t: Type) -> Self::Ty;
        fn resolve(t: Self::Ty) -> Type;
    }
}else{
    fn id_for_node_kind(kind: &str, named: bool) -> u16 {
        unimplemented!("need treesitter grammar")
    }

    pub trait CEnabledTypeStore: TypeStore {
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

impl C {
    pub const INST: C = Lang;
    pub const NAME: &'static str = "hyperast_gen_ts_c::types::Lang";
    // std::any::type_name::<Lang>() // WAITING for const_type_name feature stability
}

impl LangRef<AnyType> for C {
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
        debug_assert_eq!(std::any::type_name::<C>(), Self::NAME);
        Self::NAME
    }

    fn ts_symbol(&self, t: AnyType) -> u16 {
        // TODO check lang
        id_for_node_kind(t.as_static_str(), t.is_named())
    }
}

impl LangRef<Type> for C {
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
        debug_assert_eq!(std::any::type_name::<C>(), Self::NAME);
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

impl hyperast::types::Lang<Type> for C {
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

    fn is_error(&self) -> bool {
        self == &Type::ERROR || self == &Type::_ERROR
    }

    fn as_shared(&self) -> hyperast::types::Shared {
        use hyperast::types::Shared;
        match self {
            Type::EnumSpecifier => Shared::TypeDeclaration,
            // Type::_TypeSpecifier => Shared::TypeDeclaration, // abstract
            Type::PrimitiveType => Shared::TypeDeclaration,
            Type::SizedTypeSpecifier => Shared::TypeDeclaration,
            Type::StructSpecifier => Shared::TypeDeclaration,
            Type::TypeIdentifier => Shared::TypeDeclaration,
            Type::UnionSpecifier => Shared::TypeDeclaration,
            Type::Comment => Shared::Comment,
            Type::Identifier => Shared::Identifier,
            _ => Shared::Other,
        }
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
        let t = <C as hyperast::types::Lang<Type>>::to_u16(*self);
        let t = <C as hyperast::types::Lang<Type>>::make(t);
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
        hyperast::types::LangWrapper::from(&Lang as &(dyn LangRef<Self> + 'static))
    }

    fn lang_ref(&self) -> hyperast::types::LangWrapper<AnyType> {
        hyperast::types::LangWrapper::from(&Lang as &(dyn LangRef<AnyType> + 'static))
    }
}
impl TypeTrait for Type {
    type Lang = C;

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

const COUNT: u16 = 542;
impl Display for Type {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(self.to_str())
    }
}

impl TryFrom<&str> for Type {
    type Error = ();

    fn try_from(value: &str) -> Result<Self, Self::Error> {
        Type::from_str(value).ok_or(())
    }
}

impl Type {
    pub(crate) fn is_repeat(&self) -> bool {
        *self == Type::TranslationUnitRepeat1
            || *self == Type::PreprocParamsRepeat1
            || *self == Type::PreprocIfRepeat1
            || *self == Type::PreprocIfInFieldDeclarationListRepeat1
            || *self == Type::PreprocIfInEnumeratorListRepeat1
            || *self == Type::PreprocIfInEnumeratorListNoCommaRepeat1
            || *self == Type::PreprocArgumentListRepeat1
            || *self == Type::DeclarationRepeat1
            || *self == Type::TypeDefinitionRepeat1
            || *self == Type::_TypeDefinitionTypeRepeat1
            || *self == Type::_TypeDefinitionDeclaratorsRepeat1
            || *self == Type::_DeclarationSpecifiersRepeat1
            || *self == Type::AttributeDeclarationRepeat1
            || *self == Type::AttributedDeclaratorRepeat1
            || *self == Type::PointerDeclaratorRepeat1
            || *self == Type::ArrayDeclaratorRepeat1
            || *self == Type::SizedTypeSpecifierRepeat1
            || *self == Type::EnumeratorListRepeat1
            || *self == Type::ParameterListRepeat1
            || *self == Type::CaseStatementRepeat1
            || *self == Type::GenericExpressionRepeat1
            || *self == Type::GnuAsmExpressionRepeat1
            || *self == Type::GnuAsmOutputOperandListRepeat1
            || *self == Type::GnuAsmInputOperandListRepeat1
            || *self == Type::GnuAsmClobberListRepeat1
            || *self == Type::GnuAsmGotoListRepeat1
            || *self == Type::ArgumentListRepeat1
            || *self == Type::InitializerListRepeat1
            || *self == Type::InitializerPairRepeat1
            || *self == Type::CharLiteralRepeat1
            || *self == Type::ConcatenatedStringRepeat1
            || *self == Type::StringLiteralRepeat1
    }
}

impl hyperast::types::LLang<TType> for C {
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
impl Into<TypeU16<C>> for Type {
    fn into(self) -> TypeU16<C> {
        TypeU16::new(self)
    }
}

impl Into<u16> for Type {
    fn into(self) -> u16 {
        self as u16
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
