use hyperast::store::TyDown;
use hyperast::store::nodes::PolyglotHolder;
use hyperast::types::{
    AnyType, ETypeStore, LLang, LangRef, LangWrapper, Role, RoleStore, TypeStore, TypeTrait,
    TypeU16,
};

/// Combined type store for multiple languages.
#[derive(Clone, Copy, Default)]
pub struct TStore<CAR, CDR = EmptyMultiTStore>(CAR, CDR);

#[cfg(feature = "cpp")]
impl<CAR, CDR> TyDown<hyperast_gen_ts_cpp::TStore> for TStore<CAR, CDR> {}
#[cfg(feature = "c")]
impl<CAR, CDR> TyDown<hyperast_gen_ts_c::TStore> for TStore<CAR, CDR> {}
#[cfg(feature = "java")]
impl<CAR, CDR> TyDown<hyperast_gen_ts_java::TStore> for TStore<CAR, CDR> {}
#[cfg(feature = "python")]
impl<CAR, CDR> TyDown<hyperast_gen_ts_python::TStore> for TStore<CAR, CDR> {}
#[cfg(feature = "typescript")]
impl<CAR, CDR> TyDown<hyperast_gen_ts_typescript::TStore> for TStore<CAR, CDR> {}
#[cfg(feature = "rust")]
impl<CAR, CDR> TyDown<hyperast_gen_ts_rust::TStore> for TStore<CAR, CDR> {}
#[cfg(feature = "file_sys")]
impl<CAR, CDR> TyDown<crate::processors::file_sys::TStore> for TStore<CAR, CDR> {}
#[cfg(feature = "maven")]
impl<CAR, CDR> TyDown<hyperast_gen_ts_xml::TStore> for TStore<CAR, CDR> {}

#[doc(hidden)]
#[derive(Clone, Copy, Default)]
/// terminal case for multi language TStore
pub struct EmptyMultiTStore;

trait MultiTStore: TypeStore<Ty = AnyType> + RoleStore<Role = Role, IdF = u16> {
    fn decomp_aux(
        erazed: &impl PolyglotHolder,
        id: hyperast::store::nodes::LangId,
    ) -> Option<AnyType>;
    fn resolve_field_aux(field_id: Self::IdF, name: &str) -> Self::Role;
    fn intern_role_aux(role: Self::Role, name: &str) -> Self::IdF;
}

impl<CAR, CDR> TypeStore for TStore<CAR, CDR>
where
    CDR: MultiTStore,
    CAR: ETypeStore<Ty = TypeU16<<<CAR as ETypeStore>::Ty2 as TypeTrait>::Lang>>
        + RoleStore<IdF = u16, Role = Role>,
    <<CAR as ETypeStore>::Ty2 as TypeTrait>::Lang:
        LLang<CAR::Ty, I = u16, E = <CAR as ETypeStore>::Ty2>,
    <<CAR as ETypeStore>::Ty2 as TypeTrait>::Lang:
        LLang<TypeU16<<<CAR as ETypeStore>::Ty2 as TypeTrait>::Lang>>,
{
    type Ty = AnyType;
    fn try_decompress_type(
        erazed: &impl PolyglotHolder,
        _tid: std::any::TypeId,
    ) -> Option<Self::Ty> {
        let id = erazed.lang_id();
        Self::decomp_aux(erazed, id)
    }

    fn decompress_type(erazed: &impl PolyglotHolder, tid: std::any::TypeId) -> Self::Ty {
        if let Some(t) = Self::try_decompress_type(erazed, tid) {
            return t;
        }
        #[cfg(not(debug_assertions))]
        panic!();
        #[cfg(debug_assertions)]
        let id = erazed.lang_id();
        #[cfg(debug_assertions)]
        panic!("{} is not handled", id.name());
    }
}

impl<CAR, CDR> RoleStore for TStore<CAR, CDR>
where
    CDR: MultiTStore,
    CAR: ETypeStore<Ty = TypeU16<<<CAR as ETypeStore>::Ty2 as TypeTrait>::Lang>>
        + RoleStore<IdF = u16, Role = Role>,
    <<CAR as ETypeStore>::Ty2 as TypeTrait>::Lang: LLang<
            TypeU16<<<CAR as ETypeStore>::Ty2 as TypeTrait>::Lang>,
            I = u16,
            E = <CAR as ETypeStore>::Ty2,
        >,
{
    type IdF = u16;

    type Role = Role;

    fn resolve_field(lang: impl LangRef<Self::Ty>, field_id: Self::IdF) -> Self::Role {
        let name = lang.name();
        Self::resolve_field_aux(field_id, name)
    }

    fn intern_role(lang: impl LangRef<Self::Ty>, role: Self::Role) -> Self::IdF {
        let name = lang.name();
        Self::intern_role_aux(role, name)
    }
}

impl TypeStore for EmptyMultiTStore {
    type Ty = AnyType;

    fn try_decompress_type(
        _erazed: &impl PolyglotHolder,
        _tid: std::any::TypeId,
    ) -> Option<Self::Ty> {
        None
    }

    fn decompress_type(erazed: &impl PolyglotHolder, _tid: std::any::TypeId) -> Self::Ty {
        #[cfg(not(debug_assertions))]
        panic!();
        #[cfg(debug_assertions)]
        let id = erazed.lang_id();
        #[cfg(debug_assertions)]
        panic!("{} is not handled", id.name());
    }
}

impl RoleStore for EmptyMultiTStore {
    type IdF = u16;
    type Role = Role;

    fn resolve_field(lang: impl LangRef<Self::Ty>, _field_id: Self::IdF) -> Self::Role {
        let name = lang.name();
        panic!("unsupported lang: {}", name);
    }

    fn intern_role(lang: impl LangRef<Self::Ty>, _role: Self::Role) -> Self::IdF {
        let name = lang.name();
        panic!("unsupported lang: {}", name);
    }
}

impl<CAR, CDR> MultiTStore for TStore<CAR, CDR>
where
    CDR: MultiTStore,
    CAR: ETypeStore<Ty = TypeU16<<<CAR as ETypeStore>::Ty2 as TypeTrait>::Lang>>
        + RoleStore<IdF = u16, Role = Role>,
    <<CAR as ETypeStore>::Ty2 as TypeTrait>::Lang: LLang<
            TypeU16<<<CAR as ETypeStore>::Ty2 as TypeTrait>::Lang>,
            I = u16,
            E = <CAR as ETypeStore>::Ty2,
        >,
{
    fn decomp_aux(
        erazed: &impl PolyglotHolder,
        id: hyperast::store::nodes::LangId,
    ) -> Option<AnyType> {
        if id.is::<<<CAR as ETypeStore>::Ty2 as TypeTrait>::Lang>() {
            let x = AnyType::from_polyglot::<<<CAR as ETypeStore>::Ty2 as TypeTrait>::Lang>(erazed);
            return Some(x.unwrap());
        } else {
            CDR::decomp_aux(erazed, id)
        }
    }

    fn resolve_field_aux(field_id: Self::IdF, name: &str) -> Self::Role {
        use hyperast::types::LLang;
        use hyperast::types::Lang;
        let l =
            <<<CAR as ETypeStore>::Ty2 as TypeTrait>::Lang as Lang<<CAR as ETypeStore>::Ty2>>::INST;
        if l.name() == name {
            let w: LangWrapper<<CAR as TypeStore>::Ty> =
                <<<CAR as ETypeStore>::Ty2 as TypeTrait>::Lang>::as_lang_wrapper();
            return CAR::resolve_field(w, field_id);
        } else {
            CDR::resolve_field_aux(field_id, name)
        }
    }

    fn intern_role_aux(role: Self::Role, name: &str) -> Self::IdF {
        use hyperast::types::LLang;
        use hyperast::types::Lang;
        let l =
            <<<CAR as ETypeStore>::Ty2 as TypeTrait>::Lang as Lang<<CAR as ETypeStore>::Ty2>>::INST;
        if l.name() == name {
            let w: LangWrapper<<CAR as TypeStore>::Ty> =
                <<<CAR as ETypeStore>::Ty2 as TypeTrait>::Lang>::as_lang_wrapper();
            CAR::intern_role(w, role)
        } else {
            CDR::intern_role_aux(role, name)
        }
    }
}

impl MultiTStore for EmptyMultiTStore {
    fn decomp_aux(
        _erazed: &impl PolyglotHolder,
        _id: hyperast::store::nodes::LangId,
    ) -> Option<AnyType> {
        None
    }

    fn resolve_field_aux(_field_id: Self::IdF, name: &str) -> Self::Role {
        panic!("unsupported lang: {}", name);
    }

    fn intern_role_aux(_role: Self::Role, name: &str) -> Self::IdF {
        panic!("unsupported lang: {}", name);
    }
}
