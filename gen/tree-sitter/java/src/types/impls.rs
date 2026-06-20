//! Structs and implementations related to nodes types.
//! Long term the content of this file won't be generated, but factorized.

use std::fmt::Display;

use hyperast::tree_gen::{TsEnableTS, TsType};
use hyperast::types::{AnyType, HyperType, LangRef, TypeTrait, TypedNodeId};
use hyperast::types::{NodeId, UniformNodeId};

use super::Role;
use super::TIdN;
use super::TStore;
use super::TType;
use super::{Java, Lang};
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
        self == Type::StringLiteral
    }
}

use hyperast::types::TypeStore;
impl TypeStore for TStore {
    type Ty = TType;
}
impl TypeStore for &TStore {
    type Ty = TType;
}

impl hyperast::types::ETypeStore for TStore {
    type Ty2 = Type;

    fn intern(ty: Self::Ty2) -> Self::Ty {
        TType::new(ty)
    }
}

cfg_if::cfg_if! {if #[cfg(feature = "impl")] {
    use hyperast::types::LangWrapper;
    use hyperast::types::RoleStore;

    // static dynamically initialized once association table from Roles to tree_sitter_java Fields
    static ROLE2FIELD: std::sync::LazyLock<Box<[u16]>> = std::sync::LazyLock::new(|| {
        (0..Role::len())
            .map(|i| {
                let i = i as u8;
                let role: Role = unsafe { std::mem::transmute(i) };
                let field_name = role.to_string();
                // dbg!(&field_name);
                crate::language()
                    .field_id_for_name(field_name)
                    .map_or(u16::MAX, |x| x.into())
            })
            .collect()
    });

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
            let r = ROLE2FIELD[role as usize];
            assert!(r < u16::MAX, "Role not found");
            r
        }
    }
    impl JavaEnabledTypeStore for TStore {
        fn resolve(t: Self::Ty) -> Type {
            t.e()
        }
    }
    #[cfg(test)]
    pub fn as_any(t: &Type) -> AnyType {
        let t = <Java as hyperast::types::Lang<Type>>::to_u16(*t);
        let t = <Java as hyperast::types::Lang<Type>>::make(t);
        let t: &'static dyn HyperType = t;
        t.into()
    }
    pub trait JavaEnabledTypeStore:
        hyperast::types::ETypeStore<Ty2 = Type> + Clone + hyperast::tree_gen::TsEnableTS
    {
        fn resolve(t: Self::Ty) -> Type;
    }
    fn id_for_node_kind(kind: &str, named: bool) -> u16 {
        crate::language().id_for_node_kind(kind, named)
    }
} else {
    pub trait JavaEnabledTypeStore: hyperast::types::ETypeStore<Ty2 = Type> + Clone {
        fn resolve(t: Self::Ty) -> Type;
    }
    fn id_for_node_kind(_kind: &str, _named: bool) -> u16 {
        unimplemented!("need treesitter grammar")
    }
}}

impl<IdN: Clone + Eq + hyperast::types::UniformNodeId> NodeId for TIdN<IdN> {
    type IdN = IdN;

    fn as_id(&self) -> &Self::IdN {
        &self.0
    }

    unsafe fn from_id(id: Self::IdN) -> Self {
        Self(id)
    }

    unsafe fn from_ref_id(id: &Self::IdN) -> &Self {
        unsafe { std::mem::transmute(id) }
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
    pub const NAME: &'static str = "hyperast_gen_ts_java::types::Lang";
    // std::any::type_name::<Lang>() // WAITING for const_type_name feature stability
}

impl hyperast::types::Lang<Type> for Java {
    const INST: Self = Lang;
    fn make(t: u16) -> &'static Type {
        Lang.make(t)
    }
    fn to_u16(t: Type) -> u16 {
        Lang.to_u16(t)
    }
}

impl LangRef<Type> for Java {
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
        debug_assert_eq!(std::any::type_name::<Java>(), Self::NAME);
        Self::NAME
    }

    fn ts_symbol(&self, t: Type) -> u16 {
        assert!(t != Type::Spaces || t != Type::Directory);
        debug_assert_eq!(t as u16, id_for_node_kind(t.as_static_str(), t.is_named()));
        t as u16
    }
}
impl LangRef<AnyType> for Java {
    fn make(&self, t: u16) -> &'static AnyType {
        todo!("{}", t)
    }
    fn to_u16(&self, t: AnyType) -> u16 {
        let t: &Type = t.as_any().downcast_ref().unwrap();
        Lang.to_u16(*t)
    }

    fn name(&self) -> &'static str {
        debug_assert_eq!(std::any::type_name::<Java>(), Self::NAME);
        Self::NAME
    }

    fn ts_symbol(&self, t: AnyType) -> u16 {
        let t: Type = *t.as_any().downcast_ref().unwrap();
        assert!(t != Type::Spaces && t != Type::Directory);
        if cfg!(debug_assertions) && t != Type::Throws_ && t != Type::Permits_ {
            assert_eq!(
                Lang.to_u16(t),
                id_for_node_kind(t.as_static_str(), t.is_named()),
                "{}",
                t.as_static_str()
            );
        }
        Lang.to_u16(t)
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
        debug_assert_eq!(std::any::type_name::<Java>(), Self::NAME);
        Self::NAME
    }

    fn ts_symbol(&self, t: TType) -> u16 {
        let t: Type = *t.as_any().downcast_ref().unwrap();
        assert!(t != Type::Spaces || t != Type::Directory);
        // debug_assert_eq!(
        //     Lang.to_u16(t),
        //     id_for_node_kind(t.as_static_str(), t.is_named())
        // );
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
    }
    fn is_hidden(&self) -> bool {
        self.is_hidden()
    }
    fn is_supertype(&self) -> bool {
        self.is_supertype()
    }
    fn is_syntax(&self) -> bool {
        self.is_syntax()
    }

    fn as_shared(&self) -> hyperast::types::Shared {
        use hyperast::types::Shared;
        super::more::ty!(self,
            [self.is_error()] => Shared::Error,
            [self.is_type_declaration()] => Shared::TypeDeclaration,
            [self.is_literal()] => Shared::Literal,
            [self.is_fork()] => Shared::Branch,
            [self.is_comment()] => Shared::Comment,
            [self.is_identifier()] => Shared::Identifier,
            _ => Shared::Other,
        )
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
        LangRef::<Type>::make(&Lang, *self as u16)
    }

    fn as_static_str(&self) -> &'static str {
        self.to_str()
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
impl Display for Type {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.to_str())
    }
}

impl<'a> From<&'a str> for Type {
    fn from(value: &'a str) -> Self {
        Type::from_str(value).unwrap()
    }
}

impl hyperast::types::LLang<TType> for Java {
    type I = u16;

    type E = Type;

    const TE: &[Self::E] = S_T_L;

    fn as_lang_wrapper() -> hyperast::types::LangWrapper<TType> {
        From::<&'static dyn LangRef<_>>::from(&Lang)
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
