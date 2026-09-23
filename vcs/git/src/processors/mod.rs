use hyperast::tree_gen::extra_pattern_precomp::PrecompQueries;

// # languages
#[cfg(feature = "c")]
pub mod c;
#[cfg(feature = "cpp")]
pub mod cpp;
#[cfg(feature = "java")]
pub mod java;
#[cfg(feature = "python")]
pub mod python;
#[cfg(feature = "rust")]
pub mod rust;
#[cfg(feature = "typescript")]
pub mod typescript;

// # build systems
#[cfg(feature = "make")]
pub mod make;
#[cfg(feature = "maven")]
pub mod maven;

#[cfg(feature = "file_sys")]
pub mod file_sys;

#[derive(Clone, Debug)]
pub struct FullNode {
    pub id: hyperast::store::defaults::NodeIdentifier,
    pub metrics: crate::DefaultMetrics,
    pub precomp_queries: PrecompQueries,
}

impl crate::preprocessed::IdHolder for FullNode {
    type Id = hyperast::store::defaults::NodeIdentifier;
    fn id(&self) -> Self::Id {
        self.id
    }
}

pub type Local = hyperast::tree_gen::zipped_ts_extra::Local;

pub type FNode<E = hyperast::tree_gen::zipped_ts_extra::EmptyExtra> =
    hyperast::tree_gen::extra::NodeWithExtra<hyperast::tree_gen::zipped_ts_extra::FNode<Local>, E>;

impl From<FNode<PrecompQueries>> for FullNode {
    fn from(full_node: FNode<PrecompQueries>) -> Self {
        Self {
            id: full_node.local.compressed_node,
            metrics: full_node.local.metrics,
            precomp_queries: full_node.extra,
        }
    }
}

impl From<FNode> for FullNode {
    fn from(full_node: FNode) -> Self {
        Self {
            id: full_node.local.compressed_node,
            metrics: full_node.local.metrics,
            precomp_queries: PrecompQueries::full(),
        }
    }
}

pub struct ProcessorCache<MD = PrecompQueries, N = FullNode> {
    pub(crate) md_cache:
        hyperast::compat::HashMap<hyperast::store::nodes::legion::NodeIdentifier, MD>,
    pub(crate) dedup: hyperast::store::nodes::legion::DedupMap,
    pub object_map: crate::processing::caches::NamedMap<N>,
}

impl<MD, N> Default for ProcessorCache<MD, N> {
    fn default() -> Self {
        Self {
            md_cache: Default::default(),
            dedup: Default::default(),
            object_map: Default::default(),
        }
    }
}

impl<MD, N> crate::processing::ObjectMapper for ProcessorCache<MD, N> {
    type K = (git2::Oid, crate::processing::ObjectName);

    type V = N;

    fn get(&self, key: &Self::K) -> Option<&Self::V> {
        self.object_map.get(key)
    }

    fn insert(&mut self, key: Self::K, value: Self::V) -> Option<Self::V> {
        self.object_map.insert(key, value)
    }
}

fn prepare_dir_exploration(tree: &git2::Tree) -> impl Iterator<Item = crate::git::BasicGitObject> {
    tree.iter()
        .rev()
        .map(TryInto::try_into)
        .filter_map(|x| x.ok())
}

#[derive(Clone)]
pub(crate) struct Query(pub(crate) hyperast_tsquery::Query, crate::Str);

impl PartialEq for Query {
    fn eq(&self, other: &Self) -> bool {
        self.1 == other.1
    }
}
impl Eq for Query {}

impl Query {
    pub(crate) fn new<'a>(
        precomputeds: impl Iterator<Item = &'a str>,
        language: tree_sitter::Language,
    ) -> Self {
        use crate::precomp_patterns::only_parse_query_precomp;
        let precomputeds = precomputeds.collect::<Vec<_>>();
        let precomp = only_parse_query_precomp(precomputeds.as_slice(), language);
        Self(precomp.unwrap(), precomputeds.join("\n").into())
    }
}

static VAR_NAME: &'static str = "CST contains parsing errors";
macro_rules! report_or_fail_on_errored_tree {
    ($name:expr, $tree:expr, $parsing_time:expr) => {
        if $tree.root_node().has_error() {
            log::warn!("bad CST: {:?}", $name.try_str());
            if crate::PROPAGATE_ERROR_ON_BAD_CST_NODE {
                return Err(crate::utils::FailedParsing {
                    parsing_time: $parsing_time,
                    tree: $tree,
                    error: crate::processors::VAR_NAME,
                })?;
            }
        };
    };
}
use report_or_fail_on_errored_tree;
