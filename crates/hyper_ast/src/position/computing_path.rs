//! Gather most of the common behaviors used to compute a path from an offset

use crate::types::{Childrn as _, WithChildren as _};
use crate::types::{HyperAST, LendT, WithSerialization};

pub fn child_by_name<'store, HAST>(
    stores: &'store HAST,
    x: HAST::IdN,
    name: &str,
) -> Option<(HAST::IdN, usize)>
where
    HAST: HyperAST,
{
    let n = stores.resolve(&x);
    for (i, x) in n.children().unwrap().enumerate() {
        let c = stores.resolve(&x);
        let l = {
            use crate::types::LabelStore;
            use crate::types::Labeled;
            stores.label_store().resolve(c.get_label_unchecked())
        };
        if l == name {
            return Some((x, i));
        }
    }
    None
}

pub fn child_at_path<'a, 'store, HAST>(
    stores: &'store HAST,
    mut d: HAST::IdN,
    path: impl Iterator<Item = &'a str>,
) -> Option<HAST::IdN>
where
    HAST: HyperAST,
{
    for name in path {
        if name.trim().is_empty() {
            continue;
        }
        d = child_by_name(stores, d, name)?.0
    }
    Some(d)
}

pub fn child_at_path_with_offsets<'a, 'store, HAST>(
    stores: &'store HAST,
    mut d: HAST::IdN,
    path: impl Iterator<Item = &'a str>,
) -> Option<(HAST::IdN, Vec<usize>)>
where
    HAST: HyperAST,
{
    let mut p = vec![];
    for name in path {
        if name.trim().is_empty() {
            continue;
        }
        let cbn = child_by_name(stores, d, name)?;
        p.push(cbn.1);
        d = cbn.0
    }
    Some((d, p))
}

pub fn child_at_offsets<'a, 'store, HAST>(
    stores: &'store HAST,
    mut d: HAST::IdN,
    path: impl Iterator<Item = HAST::Idx>,
) -> Option<HAST::IdN>
where
    HAST: HyperAST,
{
    for i in path {
        let n = stores.resolve(&d);
        use crate::types::Children;
        d = n.children().unwrap().get(i).unwrap().clone();
    }
    Some(d)
}

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
        if cs.is_empty() {
            break;
        };
        for (y, child_id) in cs.enumerate() {
            let b = stores.resolve(&child_id);
            let len = b.try_bytes_len().unwrap_or(0);
            if offset < start {
                // not yet reached something
            } else if let Some(end) = end
                && offset + len <= end
            {
                break 'main;
            } else if end.is_none() {
                offsets.push(y);
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

/// Prefer using [`hyperast::position::conversions_impls::WithHyperAstPositionConverter::compute_pos_file_and_offset`]
pub fn resolve_path<'store, HAST>(
    stores: &'store HAST,
    root: HAST::IdN,
    file: &str,
    start: usize,
    end: Option<usize>,
) -> (HAST::IdN, Vec<usize>)
where
    HAST: HyperAST,
    for<'t> LendT<'t, HAST>: WithSerialization,
    HAST::IdN: Copy,
{
    let file = file.split("/");
    use crate::position::computing_path::child_at_path_with_offsets;
    let (file_node, path) = child_at_path_with_offsets(stores, root, file).unwrap();
    let (node, offsets_in_file) = resolve_range(file_node, start, end, stores);
    let offsets = path.into_iter().chain(offsets_in_file.into_iter());
    (node, offsets.collect())
}
