use hyperast::types::{AnyType, LangRef, RoleStore, TypeStore};

mod multitstore {
    // Do not hesitate to use it as an example.
    // Copy and modify it, to handle the languages you need.
    // Note: you can wrap it like I did in the rest of this file, to avoid ugly diagnostics caused by the recursive type.
    hyperast::multitstore! {
        #[cfg(feature = "java")]
        hyperast_gen_ts_java::TStore,
        #[cfg(feature = "cpp")]
        hyperast_gen_ts_cpp::TStore,
        #[cfg(feature = "c")]
        hyperast_gen_ts_c::TStore,
        #[cfg(feature = "maven")]
        hyperast_gen_ts_xml::TStore,
        #[cfg(feature = "typescript")]
        hyperast_gen_ts_typescript::TStore,
        #[cfg(feature = "rust")]
        hyperast_gen_ts_rust::TStore,
        #[cfg(feature = "python")]
        hyperast_gen_ts_python::TStore,
        #[cfg(feature = "file_sys")]
        crate::processors::file_sys::TStore
    }
}

pub use multitstore::TStore as MultiTStore;

#[derive(Clone, Copy, Default)]
pub struct TStore(MultiTStore);

#[cfg(feature = "cpp")]
impl hyperast::store::TyDown<hyperast_gen_ts_cpp::TStore> for TStore {}
#[cfg(feature = "c")]
impl hyperast::store::TyDown<hyperast_gen_ts_c::TStore> for TStore {}
#[cfg(feature = "java")]
impl hyperast::store::TyDown<hyperast_gen_ts_java::TStore> for TStore {}
#[cfg(feature = "python")]
impl hyperast::store::TyDown<hyperast_gen_ts_python::TStore> for TStore {}
#[cfg(feature = "typescript")]
impl hyperast::store::TyDown<hyperast_gen_ts_typescript::TStore> for TStore {}
#[cfg(feature = "rust")]
impl hyperast::store::TyDown<hyperast_gen_ts_rust::TStore> for TStore {}
#[cfg(feature = "file_sys")]
impl hyperast::store::TyDown<crate::processors::file_sys::TStore> for TStore {}
#[cfg(feature = "maven")]
impl hyperast::store::TyDown<hyperast_gen_ts_xml::TStore> for TStore {}

impl TypeStore for TStore {
    type Ty = AnyType;
    fn try_decompress_type(
        erazed: &impl hyperast::store::nodes::PolyglotHolder,
        _tid: std::any::TypeId,
    ) -> Option<Self::Ty> {
        MultiTStore::try_decompress_type(erazed, _tid)
    }

    fn decompress_type(
        erazed: &impl hyperast::store::nodes::PolyglotHolder,
        tid: std::any::TypeId,
    ) -> Self::Ty {
        MultiTStore::decompress_type(erazed, tid)
    }
}

impl RoleStore for TStore {
    type IdF = u16;

    type Role = hyperast::types::Role;

    fn resolve_field(lang: impl LangRef<Self::Ty>, field_id: Self::IdF) -> Self::Role {
        MultiTStore::resolve_field(lang, field_id)
    }

    fn intern_role(lang: impl LangRef<Self::Ty>, role: Self::Role) -> Self::IdF {
        MultiTStore::intern_role(lang, role)
    }
}
