use std::fmt::Display;

use hyperast::tree_gen::{TsEnableTS, TsType};
use hyperast::types::{AnyType, TypeU16};
use hyperast::types::{HyperType, LangRef, TypeStore, TypeTrait, TypedNodeId};
use hyperast::types::{NodeId, UniformNodeId};

use super::TIdN;
use super::TStore;
use super::TType;
use super::{Lang, Xml};
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
        self == Type::AttValue
    }
}

impl TypeStore for TStore {
    type Ty = TypeU16<Xml>;
}
impl TypeStore for &TStore {
    type Ty = TypeU16<Xml>;
}

// #[cfg(feature = "impl")]
// impl XmlEnabledTypeStore for TStore {
//     fn resolve(t: Self::Ty) -> Type {
//         t.e()
//     }
// }

// static dynamically initialized once association table between Role and tree_sitter_java Fields
#[cfg(feature = "impl")]
static ROLE2FIELD: std::sync::LazyLock<Box<[u16]>> = std::sync::LazyLock::new(|| {
    (0..hyperast::types::Role::len())
        .map(|i| {
            let i = i as u8;
            let role: hyperast::types::Role = unsafe { std::mem::transmute(i) };
            let field_name = role.to_string();
            // dbg!(&field_name);
            crate::language()
                .field_id_for_name(field_name)
                .map_or(u16::MAX, |x| x.into())
        })
        .collect()
});

#[cfg(feature = "impl")]
impl hyperast::types::RoleStore for TStore {
    type IdF = u16;

    type Role = super::Role;

    fn resolve_field(
        _lang: hyperast::types::LangWrapper<Self::Ty>,
        field_id: Self::IdF,
    ) -> Self::Role {
        let s = crate::language()
            .field_name_for_id(field_id)
            .ok_or_else(|| format!("{}", field_id))
            .unwrap();
        super::Role::try_from(s).expect(s)
    }

    fn intern_role(_lang: hyperast::types::LangWrapper<Self::Ty>, role: Self::Role) -> Self::IdF {
        let r = ROLE2FIELD[role as usize];
        assert!(r < u16::MAX, "Role not found");
        r
    }
}

#[cfg(feature = "impl")]
fn id_for_node_kind(kind: &str, named: bool) -> u16 {
    crate::language().id_for_node_kind(kind, named)
}

#[cfg(not(feature = "impl"))]
fn id_for_node_kind(_kind: &str, _named: bool) -> u16 {
    unimplemented!("need treesitter grammar")
}

#[cfg(test)]
pub fn as_any(t: &Type) -> AnyType {
    let t = <Xml as hyperast::types::Lang<Type>>::to_u16(*t);
    let t = <Xml as hyperast::types::Lang<Type>>::make(t);
    let t: &'static dyn HyperType = t;
    t.into()
}

// #[cfg(not(feature = "impl"))]
// pub trait XmlEnabledTypeStore: hyperast::types::ETypeStore<Ty2 = Type> {
//     fn resolve(t: Self::Ty) -> Type;
// }

// #[cfg(feature = "impl")]
// pub trait XmlEnabledTypeStore:
//     hyperast::types::ETypeStore<Ty2 = Type> + hyperast::tree_gen::utils_ts::TsEnableTS
// {
//     fn resolve(t: Self::Ty) -> Type;
// }

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

impl Lang {
    pub const NAME: &'static str = "hyperast_gen_ts_xml::types::Lang";
    // std::any::type_name::<Lang>() // WAITING for const_type_name feature stability
}

impl hyperast::types::Lang<Type> for Xml {
    const INST: Self = Lang;
    fn make(t: u16) -> &'static Type {
        Lang.make(t)
    }
    fn to_u16(t: Type) -> u16 {
        Lang.to_u16(t)
    }
}

impl LangRef<Type> for Xml {
    fn name(&self) -> &'static str {
        debug_assert_eq!(std::any::type_name::<Xml>(), Self::NAME);
        Self::NAME
    }

    fn make(&self, t: u16) -> &'static Type {
        if t == TStore::ERROR {
            &Type::ERROR
        } else if t == TStore::_ERROR {
            &Type::_ERROR
        } else if t == TStore::SPACES {
            &Type::Spaces
        } else if t == TStore::DIRECTORY {
            &Type::Directory
        } else if t == TStore::META_DIR {
            &Type::MavenDirectory
        } else {
            &S_T_L[t as usize]
        }
    }

    fn to_u16(&self, t: Type) -> u16 {
        t as u16
    }

    fn ts_symbol(&self, t: Type) -> u16 {
        assert!(t != Type::Spaces || t != Type::Directory);
        debug_assert_eq!(
            Lang.to_u16(t),
            id_for_node_kind(t.as_static_str(), t.is_named())
        );
        Lang.to_u16(t)
    }
}

impl LangRef<AnyType> for Xml {
    fn name(&self) -> &'static str {
        debug_assert_eq!(std::any::type_name::<Xml>(), Self::NAME);
        Self::NAME
    }

    fn make(&self, _t: u16) -> &'static AnyType {
        todo!()
    }

    fn to_u16(&self, t: AnyType) -> u16 {
        let t: &Type = t.as_any().downcast_ref().unwrap();
        Lang.to_u16(*t)
    }

    fn ts_symbol(&self, t: AnyType) -> u16 {
        let t: Type = *t.as_any().downcast_ref().unwrap();
        assert!(t != Type::Spaces && t != Type::Directory && t != Type::MavenDirectory);
        debug_assert_eq!(
            Lang.to_u16(t),
            id_for_node_kind(t.as_static_str(), t.is_named()),
            "{}",
            t.as_static_str()
        );
        Lang.to_u16(t)
    }
}

impl LangRef<hyperast::types::TypeU16<Self>> for Lang {
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

    fn as_shared(&self) -> hyperast::types::Shared {
        use hyperast::types::Shared;
        if self.is_error() {
            return Shared::Error;
        }
        Shared::Other
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
        let t = <Xml as hyperast::types::Lang<Type>>::to_u16(*self);

        (<Xml as hyperast::types::Lang<Type>>::make(t)) as _
    }

    fn as_static_str(&self) -> &'static str {
        self.to_str()
    }

    fn is_file(&self) -> bool {
        self == &Type::Document
    }

    fn is_directory(&self) -> bool {
        self == &Type::Directory || self == &Type::MavenDirectory
    }

    fn is_spaces(&self) -> bool {
        self == &Type::Spaces
    }

    fn is_syntax(&self) -> bool {
        self.is_syntax()
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
    type Lang = Xml;

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

impl Type {
    pub fn resolve(t: u16) -> Self {
        assert!(t < COUNT);
        unsafe { std::mem::transmute(t) }
    }
}
const COUNT: u16 = 136 + 1 + 3;

impl Display for Type {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(self.to_str())
    }
}

impl TryFrom<&str> for Type {
    type Error = String;

    fn try_from(value: &str) -> Result<Self, <Self as TryFrom<&str>>::Error> {
        Type::from_str(value).ok_or_else(|| value.to_owned())
    }
}

impl hyperast::types::LLang<hyperast::types::TypeU16<Self>> for Xml {
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

impl From<Type> for TypeU16<Xml> {
    fn from(val: Type) -> Self {
        TypeU16::new(val)
    }
}

impl From<Type> for u16 {
    fn from(val: Type) -> Self {
        val as u8 as u16
    }
}
