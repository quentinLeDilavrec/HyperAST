//! minor impls

use super::*;

impl Parameter {
    pub fn new(query: impl hyperast_tsquery::ArrayStr) -> Self {
        Self {
            query: Some(query.iter().collect()),
        }
    }
}

impl CAcc {
    pub(crate) fn new(name: String) -> Self {
        Self {
            primary: DirPrimary::new(name),
            precomp_queries: PrecompQueries::default(),
        }
    }
}

impl PartialEq<CProc> for Parameter {
    fn eq(&self, other: &CProc) -> bool {
        self == &other.parameter
    }
}

impl Into<CProc> for Parameter {
    fn into(self) -> CProc {
        let query = self.query.as_ref().map(|q| {
            use hyperast_tsquery::ArrayStr;
            crate::processors::Query::new(q.iter(), hyperast_gen_ts_c::language())
        });
        CProc {
            parameter: self,
            query,
            cache: Default::default(),
            commits: Default::default(),
        }
    }
}

impl From<String> for CAcc {
    fn from(name: String) -> Self {
        Self::new(name)
    }
}

impl From<hyperast_gen_ts_c::legion::Local> for FullNode {
    fn from(full_node: hyperast_gen_ts_c::legion::Local) -> Self {
        Self {
            id: full_node.compressed_node,
            metrics: full_node.metrics,
            precomp_queries: PrecompQueries(full_node.precomp_queries),
        }
    }
}
