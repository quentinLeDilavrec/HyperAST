mod hash_store;
pub mod mapping_store;
mod multi_hash_store;
pub mod multi_vec_store;
pub mod vec_store;

pub use mapping_store::{MappingStore, MonoMappingStore, MultiMappingStore};
pub use multi_vec_store::MultiVecStore;
pub use vec_store::VecStore;

pub type DefaultMappingStore<T> = VecStore<T>;
pub type DefaultMultiMappingStore<T> = MultiVecStore<T>;
