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
