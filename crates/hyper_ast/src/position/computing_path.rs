//! Gather most of the common behaviors used to compute a path from an offset

use crate::types::{Childrn as _, WithChildren as _};
use crate::types::{HyperAST, LendT, WithSerialization};

/// must be in a file
pub fn resolve_range<'store, HAST>(
    root: HAST::IdN,
    start: usize,
    end: Option<usize>,
    stores: &'store HAST,
) -> (HAST::IdN, Vec<usize>)
where
    HAST: HyperAST,
    for<'t> LendT<'t, HAST>: WithSerialization,
    HAST::IdN: Copy,
{
    let mut offset = 0;
    let mut x = root;
    let mut offsets = vec![];
    'main: loop {
        let b = stores.resolve(&x);
        let Some(cs) = b.children() else {
            break;
        };
        for (y, child_id) in cs.iter_children().enumerate() {
            let b = stores.resolve(&child_id);
            let len = b.try_bytes_len().unwrap_or(0);
            if offset + len < start {
                // not yet reached something
            } else if end.is_none_or(|end| offset + len <= end) {
                break 'main;
            } else {
                offsets.push(y);
                x = child_id;
                break;
            }
            offset += len;
        }
    }
    (x, offsets)
}
