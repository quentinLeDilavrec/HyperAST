//! minor impls

use super::*;

impl Parameter {
    pub fn new(query: impl hyperast_tsquery::ArrayStr) -> Self {
        Self {
            query: Some(query.iter().collect()),
        }
    }
}

impl CppAcc {
    pub(crate) fn new(name: String) -> Self {
        Self {
            primary: DirPrimary::new(name),
            precomp_queries: PrecompQueries::default(),
        }
    }
}

impl PartialEq<CppProc> for Parameter {
    fn eq(&self, other: &CppProc) -> bool {
        self == &other.parameter
    }
}

impl Into<CppProc> for Parameter {
    fn into(self) -> CppProc {
        let query = self.query.as_ref().map(|q| {
            use hyperast_tsquery::ArrayStr;
            crate::processors::Query::new(q.iter(), hyperast_gen_ts_cpp::language())
        });
        CppProc {
            parameter: self,
            query,
            cache: Default::default(),
            commits: Default::default(),
        }
    }
}

impl From<String> for CppAcc {
    fn from(name: String) -> Self {
        Self::new(name)
    }
}

impl From<hyperast_gen_ts_cpp::legion::Local> for FullNode {
    fn from(full_node: hyperast_gen_ts_cpp::legion::Local) -> Self {
        Self {
            id: full_node.compressed_node,
            metrics: full_node.metrics,
            precomp_queries: PrecompQueries(full_node.precomp_queries),
        }
    }
}
