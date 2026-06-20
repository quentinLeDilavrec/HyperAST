use hyperast::types::{AnyType, HyperType, LangRef, LangWrapper, RoleStore, TypeStore};

#[derive(Clone, Copy)]
pub struct TStore;

#[cfg(feature = "cpp")]
impl hyperast::store::TyDown<hyperast_gen_ts_cpp::TStore> for TStore {}
#[cfg(feature = "java")]
impl hyperast::store::TyDown<hyperast_gen_ts_java::TStore> for TStore {}
#[cfg(feature = "python")]
impl hyperast::store::TyDown<hyperast_gen_ts_python::TStore> for TStore {}
// #[cfg(feature = "file_sys")]
// impl hyperast::store::TyDown<crate::processors::file_sys::TStore> for TStore {}
#[cfg(feature = "maven")]
impl hyperast::store::TyDown<hyperast_gen_ts_xml::TStore> for TStore {}

impl Default for TStore {
    fn default() -> Self {
        Self
    }
}

impl RoleStore for TStore {
    type IdF = u16;

    type Role = hyperast::types::Role;

    fn resolve_field(lang: LangWrapper<Self::Ty>, field_id: Self::IdF) -> Self::Role {
        let name = lang.name();
        macro_rules! resolve_field {
            ($lang:path) => {{
                use $lang as l;
                if let l::Lang::NAME = name {
                    let t = l::TType::new(l::Type::Spaces);
                    return l::TStore::resolve_field(t.get_lang(), field_id);
                }
            }};
        }
        #[cfg(feature = "java")]
        resolve_field!(hyperast_gen_ts_java);
        #[cfg(feature = "cpp")]
        resolve_field!(hyperast_gen_ts_cpp);
        #[cfg(feature = "maven")]
        resolve_field!(hyperast_gen_ts_xml);
        panic!("unsupported lang: {}", name);
    }

    fn intern_role(lang: LangWrapper<Self::Ty>, role: Self::Role) -> Self::IdF {
        // TODO fix that, the lang thing, both parameter and the get_lang() should be respectively extracted and removed
        let name = lang.name();
        macro_rules! intern_role {
            ($lang:path) => {{
                use $lang as l;
                if let l::Lang::NAME = name {
                    let t = l::TType::new(l::Type::Spaces);
                    return l::TStore::intern_role(t.get_lang(), role);
                }
            }};
        }
        #[cfg(feature = "java")]
        intern_role!(hyperast_gen_ts_java);
        #[cfg(feature = "cpp")]
        intern_role!(hyperast_gen_ts_cpp);
        #[cfg(feature = "maven")]
        intern_role!(hyperast_gen_ts_xml);
        panic!("unsupported lang: {}", name);
    }
}

impl TypeStore for TStore {
    type Ty = AnyType;
    fn try_decompress_type(
        erazed: &impl hyperast::store::nodes::PolyglotHolder,
        _tid: std::any::TypeId,
    ) -> Option<Self::Ty> {
        use HyperType;
        let id = erazed.lang_id();
        macro_rules! decomp_t {
            ($p:path) => {{
                use $p as types;
                if id.is::<types::Lang>() {
                    let tid = std::any::TypeId::of::<types::TType>();
                    return erazed
                        .unerase_ref::<types::TType>(tid)
                        .map(|x| *x)
                        .map(|x| x.as_static().into());
                }
            }};
        }
        decomp_t!(hyperast_gen_ts_java);
        decomp_t!(hyperast_gen_ts_cpp);
        decomp_t!(hyperast_gen_ts_xml);
        None
    }
    fn decompress_type(
        erazed: &impl hyperast::store::nodes::PolyglotHolder,
        _tid: std::any::TypeId,
    ) -> Self::Ty {
        if let Some(t) = Self::try_decompress_type(erazed, _tid) {
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
