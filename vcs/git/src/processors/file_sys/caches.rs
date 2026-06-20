use hyperast::store::defaults::NodeIdentifier;
use hyperast::tree_gen::extra_pattern_precomp::PatternPrecompExtra;
use hyperast_gen_ts_python::TStore;

use crate::processing::ObjectMapper;
use crate::processing::caches::OidMap;

use super::PrecompQueries;

type Local = hyperast_gen_ts_python::legion::Local;
type Acc = hyperast_gen_ts_python::legion::Acc;
type PrepQuery<'q> = hyperast_tsquery::PreparedQuerying<&'q hyperast_tsquery::Query, TStore, Acc>;
type Extra<'a> = PatternPrecompExtra<NodeIdentifier, Acc, PrepQuery<'a>>;

type MDCache = hyperast::compat::HashMap<NodeIdentifier, PrecompQueries>;

#[derive(Default)]
pub struct FileSys {
    pub(crate) md_cache: MDCache,
    /// Passed to subtree builder when deriving different data (assumed to be incompatible).
    pub(crate) dedup: hyperast::store::nodes::legion::DedupMap,
    pub object_map: OidMap<super::FullNode>,
}

impl ObjectMapper for FileSys {
    type K = git2::Oid;

    type V = super::FullNode;

    fn get(&self, key: &Self::K) -> Option<&Self::V> {
        self.object_map.get(key)
    }

    fn insert(&mut self, key: Self::K, value: Self::V) -> Option<Self::V> {
        self.object_map.insert(key, value)
    }
}
