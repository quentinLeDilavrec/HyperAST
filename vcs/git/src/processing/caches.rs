use super::ObjectName;

#[derive(Default)]
pub struct OidHash(u64);
impl std::hash::Hasher for OidHash {
    fn finish(&self) -> u64 {
        self.0
    }

    fn write(&mut self, bytes: &[u8]) {
        self.0 = u64::from_be_bytes([
            bytes[0], bytes[1], bytes[2], bytes[3], bytes[4], bytes[5], bytes[6], bytes[7],
        ])
    }
}

type OidHasher = std::hash::BuildHasherDefault<OidHash>;

pub(crate) type OidMap<T> = hyperast::compat::HashMap<git2::Oid, T, OidHasher>;
pub(crate) type NamedMap<T> = hyperast::compat::HashMap<(git2::Oid, ObjectName), T, OidHasher>;

#[cfg(feature = "maven")]
#[derive(Default)]
pub struct Maven {
    pub object_map: OidMap<crate::processors::maven::FullNode>,
}

#[cfg(feature = "maven")]
#[derive(Default)]
pub struct Pom {
    pub object_map: OidMap<crate::processors::maven::POM>,
}

#[cfg(feature = "maven")]
impl super::ObjectMapper for Pom {
    type K = git2::Oid;

    type V = crate::processors::maven::POM;

    fn get(&self, key: &Self::K) -> Option<&Self::V> {
        self.object_map.get(key)
    }

    fn insert(&mut self, key: Self::K, value: Self::V) -> Option<Self::V> {
        self.object_map.insert(key, value)
    }
}

#[cfg(feature = "make")]
#[derive(Default)]
pub struct Make {
    pub object_map: OidMap<crate::processors::FullNode>,
}

#[cfg(feature = "make")]
#[derive(Default)]
pub struct Makefile {
    pub object_map: OidMap<crate::processors::make::MakeFile>,
}

#[cfg(feature = "make")]
impl super::ObjectMapper for Makefile {
    type K = git2::Oid;

    type V = crate::processors::make::MakeFile;

    fn get(&self, key: &Self::K) -> Option<&Self::V> {
        self.object_map.get(key)
    }

    fn insert(&mut self, key: Self::K, value: Self::V) -> Option<Self::V> {
        self.object_map.insert(key, value)
    }
}

// // any
// pub object_map_any: OidMap<(NodeIdentifier, DefaultMetrics)>,
// // maven
// #[cfg(feature = "maven")]
// pub object_map_maven: OidMap<(NodeIdentifier, crate::maven::MD)>,
// // make
// #[cfg(feature = "make")]
// pub object_map_make: OidMap<(NodeIdentifier, crate::make::MD)>,
// // npm
// #[cfg(feature = "npm")]
// pub object_map_npm: OidMap<(NodeIdentifier, DefaultMetrics)>,

// // pom.xml
// #[cfg(feature = "maven")]
// pub object_map_pom: OidMap<POM>,
// // MakeFile
// #[cfg(feature = "make")]
// pub object_map_makefile: OidMap<MakeFile>,
// // Java
// #[cfg(feature = "java")]
// pub(super) java_md_cache: java_tree_gen::MDCache,
// #[cfg(feature = "java")]
// pub object_map_java: NamedMap<(java_tree_gen::Local, IsSkippedAna)>,
// // Cpp
// #[cfg(feature = "cpp")]
// pub(super) cpp_md_cache: cpp_tree_gen::MDCache,
// #[cfg(feature = "cpp")]
// pub object_map_cpp: NamedMap<(cpp_tree_gen::Local, IsSkippedAna)>,
