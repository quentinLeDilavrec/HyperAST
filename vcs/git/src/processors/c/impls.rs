//! minor impls

use super::*;

impl From<hyperast_gen_ts_c::legion::Local> for FullNode {
    fn from(full_node: hyperast_gen_ts_c::legion::Local) -> Self {
        Self {
            id: full_node.compressed_node,
            metrics: full_node.metrics,
            precomp_queries: PrecompQueries(full_node.precomp_queries),
        }
    }
}
