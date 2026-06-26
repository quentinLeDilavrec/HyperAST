use hyperast::tree_gen::{TsEnableTS, TsType};
use hyperast::types::{AnyType, HyperType, LangRef, TypeStore, TypeTrait, TypeU16};

#[derive(Clone, Copy, Default)]
pub struct TStore;

#[derive(Debug)]
pub struct Lang;

#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub struct TIdN<IdN>(IdN);

pub type TType = hyperast::types::TypeU16<Lang>;

#[repr(u16)]
#[derive(Debug, Clone, Copy, Hash, PartialEq, Eq)]
pub enum Type {
    Directory = TStore::DIRECTORY,
    Spaces = TStore::SPACES,
    _ERROR = TStore::_ERROR,
    ERROR = TStore::ERROR,
}

impl std::fmt::Display for Type {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.as_static_str())
    }
}

impl HyperType for Type {
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

    fn generic_eq(&self, other: &dyn HyperType) -> bool
    where
        Self: 'static + Sized,
    {
        // Do a type-safe casting. If the types are different,
        // return false, otherwise test the values for equality.
        other
            .as_any()
            .downcast_ref::<Self>()
            .map_or(false, |a| self == a)
    }

    fn is_file(&self) -> bool {
        false
    }

    fn is_directory(&self) -> bool {
        self == &Type::Directory
    }

    fn is_spaces(&self) -> bool {
        false
    }

    fn is_syntax(&self) -> bool {
        false
    }

    fn is_hidden(&self) -> bool {
        false
    }

    fn is_named(&self) -> bool {
        true
    }

    fn is_supertype(&self) -> bool {
        false
    }

    fn is_error(&self) -> bool {
        false
    }

    fn as_shared(&self) -> hyperast::types::Shared {
        hyperast::types::Shared::Other
    }

    fn as_abstract(&self) -> hyperast::types::Abstracts {
        Default::default()
    }

    fn get_lang(&self) -> hyperast::types::LangWrapper<Self>
    where
        Self: Sized,
    {
        hyperast::types::LangWrapper::from(&Lang as &(dyn LangRef<Self> + 'static))
    }

    fn lang_ref(&self) -> hyperast::types::LangWrapper<hyperast::types::AnyType> {
        hyperast::types::LangWrapper::from(&Lang as &(dyn LangRef<AnyType> + 'static))
    }
}

impl TsType for Type {
    fn is_repeat(&self) -> bool {
        false
    }

    fn is_leaf(self) -> bool {
        false
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
        static LEN: u16 = 0;
        if LEN <= k && k < TStore::LOWEST_RESERVED {
            return None;
        }
        Some(Type::from_u16(k))
    }

    fn spaces() -> Type {
        panic!("no spaces at the file system level")
    }

    fn error() -> Type {
        Type::ERROR
    }

    fn file() -> Type {
        // NOTE could provide one though
        panic!("no specific file type at the file system level")
    }
}

impl Lang {
    pub const INST: Lang = Lang;
    pub const fn name() -> &'static str {
        // std::any::type_name::<Lang>() // WAITING for const_type_name feature stability
        "hyperast_vcs_git::processors::file_sys::types::Lang"
    }
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
        debug_assert_eq!(std::any::type_name::<Lang>(), Self::name());
        Self::name()
    }

    fn ts_symbol(&self, _t: AnyType) -> u16 {
        // TODO check lang
        // id_for_node_kind(t.as_static_str(), t.is_named())
        unimplemented!()
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
            panic!()
        }
    }
    fn to_u16(&self, t: Type) -> u16 {
        t as u16
    }

    fn name(&self) -> &'static str {
        debug_assert_eq!(std::any::type_name::<Lang>(), Self::name());
        Self::name()
    }

    fn ts_symbol(&self, _t: Type) -> u16 {
        unimplemented!()
        // id_for_node_kind(t.as_static_str(), t.is_named())
    }
}

impl LangRef<TType> for Lang {
    fn make(&self, _t: u16) -> &'static TType {
        todo!()
        // TODO could make one safe, but not priority
        // unsafe { std::mem::transmute(&S_T_L[t as usize]) }
    }
    fn to_u16(&self, t: TType) -> u16 {
        t.e() as u16
    }

    fn name(&self) -> &'static str {
        debug_assert_eq!(std::any::type_name::<Lang>(), Self::name());
        Self::name()
    }

    fn ts_symbol(&self, _t: TType) -> u16 {
        unimplemented!()
        // id_for_node_kind(t.as_static_str(), t.is_named())
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

impl hyperast::types::LLang<TType> for Lang {
    type I = u16;

    type E = Type;

    const TE: &[Self::E] = &[];

    fn as_lang_wrapper() -> hyperast::types::LangWrapper<TType> {
        From::<&'static dyn LangRef<_>>::from(&Lang)
    }
}

impl TypeStore for TStore {
    type Ty = TypeU16<Lang>;
}

impl hyperast::types::ETypeStore for TStore {
    type Ty2 = Type;

    fn intern(ty: Self::Ty2) -> Self::Ty {
        TType::new(ty)
    }
}

impl Type {
    pub fn from_u16(t: u16) -> Type {
        match t {
            TStore::DIRECTORY => Type::Directory,
            TStore::SPACES => Type::Spaces,
            TStore::_ERROR => Type::_ERROR,
            TStore::ERROR => Type::ERROR,
            _ => panic!(),
        }
    }
    #[allow(unreachable_patterns)]
    pub fn from_str(t: &str) -> Option<Type> {
        Some(match t {
            "Directory" => Type::Directory,
            "Spaces" => Type::Spaces,
            "_ERROR" => Type::_ERROR,
            "ERROR" => Type::ERROR,
            _ => return None,
        })
    }
    pub fn to_str(&self) -> &'static str {
        match self {
            Type::Directory => "Directory",
            Type::Spaces => "Spaces",
            Type::_ERROR => "_ERROR",
            Type::ERROR => "ERROR",
        }
    }
}
