use num::{one, zero};
use std::fmt::Debug;

use crate::position::position_accessors::RootedPosition;
use crate::types::{Children, WithChildren};
use crate::types::{HyperAST, LabelStore, NodeStore};
use crate::types::{HyperType, Labeled, LendT, NodeId};
use crate::types::{WithSerialization, WithStats};

use super::building;
use super::building::{bottom_up, top_down};
use super::{WithHyperAstPositionConverter, position_accessors};

// from a top down full path
impl<HAST, S> WithHyperAstPositionConverter<'_, '_, S, HAST>
where
    S: super::node_filter_traits::Full,
    S: position_accessors::WithFullPostOrderPath<HAST::IdN, Idx = HAST::Idx>
        + position_accessors::SolvedPosition<HAST::IdN>,
    HAST: HyperAST,
    HAST::IdN: NodeId<IdN = HAST::IdN> + Eq + Debug + Copy,
    HAST::Idx: Debug,
{
    pub fn compute_pos_post_order<O, B>(&self) -> O
    where
        for<'t> LendT<'t, HAST>: WithSerialization + WithStats,
        B: bottom_up::ReceiveInFile<HAST::IdN, HAST::Idx, usize, O> + bottom_up::CreateBuilder,
        B::SB1<O>: bottom_up::ReceiveDir<HAST::IdN, HAST::Idx, O>,
    {
        let builder: B = bottom_up::CreateBuilder::create();
        let stores = self.stores;
        let mut prev_x;
        let (mut x, mut iter) = self.src.iter_with_nodes();
        let mut o;
        let inner = {
            let b = stores.node_store().resolve(x.as_id());
            let inner = if let Some(len) = b.try_bytes_len() {
                Some((len, b.line_count()))
            } else {
                assert!(stores.resolve_type(x.as_id()).is_directory());
                None
            };
            prev_x = x;
            inner
        };
        use bottom_up::ReceiveNode;
        let mut builder: B::SB1<O> = if let Some((len, rows)) = inner {
            use building::SetLineSpan;
            let mut builder = builder.set(len).set(rows);
            let builder = loop {
                let Some(aaa) = iter.next() else {
                    use bottom_up::SetRoot;
                    return builder.set_root(prev_x);
                };
                x = aaa.1;
                o = aaa.0;
                let b = stores.node_store().resolve(x.as_id());
                let t = stores.resolve_type(x.as_id());
                let v = &b.children().unwrap();
                assert_eq!(Some(&prev_x), v.get(o));
                let v = v.before(o).collect::<Vec<_>>();
                fn compute<'store, HAST: HyperAST>(
                    stores: &'store HAST,
                    x: &HAST::IdN,
                    col: &mut usize,
                ) -> usize
                where
                    HAST::IdN: NodeId<IdN = HAST::IdN> + Eq + Debug,
                    for<'t> LendT<'t, HAST>: WithStats + WithSerialization,
                {
                    let b = stores.node_store().resolve(x);
                    let l = b.line_count();
                    if l == 0 {
                        *col += b.try_bytes_len().unwrap_or_default();
                    } else if let Some(cs) = b.children() {
                        for x in cs {
                            if compute(stores, &x, col) > 0 {
                                break;
                            }
                        }
                    } else {
                        *col += b.try_bytes_len().unwrap_or_default() - b.line_count();
                    }
                    l
                }
                let mut row = 0;
                let mut col = 0;
                for x in v.iter().rev() {
                    if row == 0 {
                        row += compute(stores, x, &mut col);
                    } else {
                        let b = stores.node_store().resolve(x);
                        row += b.line_count();
                    }
                }
                let bytes = v
                    .into_iter()
                    .map(|x| stores.resolve(&x).try_bytes_len().unwrap_or_default())
                    .sum();

                use bottom_up::{ReceiveIdx, ReceiveOffset};
                use building::{ReceiveColumns, ReceiveRows};
                builder = builder.push(prev_x).push(bytes).push(row).push(col).push(o);
                prev_x = x;

                if t.is_file() {
                    let l = stores.label_store().resolve(b.get_label_unchecked());
                    break bottom_up::ReceiveDirName::push(builder, l);
                }
            };
            builder
        } else {
            builder.transit()
        };

        loop {
            let Some(aaa) = iter.next() else {
                use bottom_up::SetRoot;
                return builder.set_root(prev_x);
            };
            x = aaa.1;
            o = aaa.0;

            use bottom_up::ReceiveIdx;
            builder = builder.push(prev_x).push(o);
            prev_x = x;
        }
    }
}

// from a bottom up full path
impl<HAST, S> WithHyperAstPositionConverter<'_, '_, S, HAST>
where
    S: super::node_filter_traits::Full,
    S: position_accessors::WithPreOrderOffsets<Idx = HAST::Idx>
        + position_accessors::RootedPosition<HAST::IdN>,
    HAST: HyperAST,
    HAST::IdN: NodeId<IdN = HAST::IdN> + Eq + Debug + Copy,
    HAST::Idx: Debug,
{
    pub fn compute_pos_pre_order<O, B>(&self) -> O
    where
        for<'t> LendT<'t, HAST>: WithSerialization + WithStats,
        B: top_down::ReceiveDir<HAST::IdN, HAST::Idx, O> + top_down::CreateBuilder<HAST::IdN>,
        B::SB1<O>: top_down::ReceiveInFile<HAST::IdN, HAST::Idx, usize, O>,
    {
        let stores = self.stores;
        let mut x = self.src.root();
        let mut it = self.src.iter_offsets();
        let mut builder: B = top_down::CreateBuilder::create(x);

        use building::SetLen;
        use top_down::ReceiveDirName;
        use top_down::ReceiveIdx;
        use top_down::ReceiveParent;
        loop {
            if stores.resolve_type(&x).is_file() {
                break;
            }
            let Some(o) = it.next() else {
                return builder.set_node(x);
            };
            let n = stores.resolve(&x);
            let cs = n.children().unwrap();
            let c = cs
                .get(o)
                .unwrap_or_else(|| panic!("{}", stores.resolve_type(&x).as_static_str()));
            let parent = x;
            x = *c;
            let idx = o;
            let dir_name = stores.label_store().resolve(n.get_label_unchecked());
            builder = builder.push(parent).push(idx).push(dir_name);
        }
        let n = stores.resolve(&x);
        let file_name = stores.label_store().resolve(n.get_label_unchecked());
        let mut builder = builder.set_file_name(file_name);

        loop {
            let Some(o) = it.next() else { break };
            let n = stores.resolve(&x);
            let cs = n.children().unwrap();
            let c = cs.get(o).unwrap();
            let parent = x;
            x = *c;
            let idx1 = o;
            let bytes = cs
                .before(o)
                .map(|x| stores.resolve(&x).try_bytes_len().unwrap_or_default())
                .sum();
            let rows = cs.before(o).map(|x| stores.resolve(&x).line_count()).sum();
            let mut no_s_idx = zero();
            for y in cs.before(o) {
                if !stores.resolve_type(&y).is_spaces() {
                    no_s_idx += one();
                }
            }
            use building::ReceiveRows;
            use top_down::ReceiveIdxNoSpace;
            use top_down::ReceiveOffset;
            builder = builder
                .push(parent)
                .push(idx1)
                .push(bytes)
                .push(no_s_idx)
                .push(rows);
        }
        let n = stores.resolve(&x);
        use building::SetLineSpan;
        use top_down::SetNode;
        if let Some(len) = n.try_bytes_len() {
            let lines = n.line_count();
            let node = x;
            builder.set(len).set(lines).set_node(node)
        } else {
            todo!()
        }
    }
}

// from a file and (bytes) offset position
impl<HAST> WithHyperAstPositionConverter<'_, '_, super::RootedPosition<HAST::IdN>, HAST>
where
    super::RootedPosition<HAST::IdN>: super::node_filter_traits::Full,
    HAST: HyperAST,
    HAST::IdN: NodeId<IdN = HAST::IdN> + Eq + Debug + Copy,
    HAST::Idx: Debug,
{
    pub fn compute_pos_file_and_offset<O, B>(&self) -> O
    where
        for<'t> LendT<'t, HAST>: WithSerialization + WithStats,
        B: top_down::ReceiveDir<HAST::IdN, HAST::Idx, O> + top_down::CreateBuilder<HAST::IdN>,
        B::SB1<O>: top_down::ReceiveInFile<HAST::IdN, HAST::Idx, usize, O>,
    {
        let stores = self.stores;
        let root = self.src.root();
        let file = self.src.inner().file().to_string_lossy();
        let file = file.as_ref();
        let start = self.src.inner().range().start;
        let end = Some(self.src.inner().range().end);
        let mut file = file.split("/");

        let mut builder: B = top_down::CreateBuilder::create(root);

        use building::SetLen;
        use top_down::ReceiveDirName;
        use top_down::ReceiveIdx;
        use top_down::ReceiveParent;

        // use crate::position::computing_path::child_at_path_with_offsets;
        // let (file_node, path) = child_at_path_with_offsets(stores, root, file).unwrap();
        let mut x = root;
        loop {
            if stores.resolve_type(&x).is_file() {
                break;
            }
            let Some(name) = file.next() else {
                return builder.set_node(x);
            };
            if name.trim().is_empty() {
                continue;
            }
            let cbn = crate::position::computing_path::child_by_name(stores, x, name).unwrap();
            let dir_name = name;
            let idx = num::cast(cbn.1).unwrap();
            let parent = x;
            x = cbn.0;
            builder = builder.push(parent).push(idx).push(dir_name);
        }

        let n = stores.resolve(&x);
        let file_name = stores.label_store().resolve(n.get_label_unchecked());
        let mut builder = builder.set_file_name(file_name);

        // let (node, offsets_in_file) = crate::position::resolve_range(file_node, start, end, stores);
        let start = start;
        let mut offset = 0;
        let mut x = x; // node
        // let mut offsets = vec![]; // offsets_in_file
        'main: loop {
            dbg!();
            let mut no_s_idx = zero();
            let mut bytes = zero();
            let mut idx1 = zero();
            let mut rows = zero();
            let parent = x;
            let b = stores.resolve(&x);
            let Some(cs) = b.children() else {
                break;
            };
            use crate::types::Childrn;
            if cs.is_empty() {
                break;
            };
            let mut cs = cs.enumerate();
            loop {
                let Some((y, child_id)) = cs.next() else {
                    break;
                };
                idx1 = num::cast(y).unwrap();
                if !stores.resolve_type(&child_id).is_spaces() {
                    no_s_idx += one();
                }
                let b = stores.resolve(&child_id);
                let line_count = b.line_count();
                let len = b.try_bytes_len().unwrap_or(0);
                rows += line_count;
                bytes += len;
                // dbg!(offset, len, start, idx1);
                if offset < start {
                    // not yet reached something
                } else if let Some(end) = end
                    && offset + len <= end
                {
                    // found
                    dbg!();
                    builder = builder
                        .push(parent)
                        .push(idx1)
                        .push(bytes)
                        .push(no_s_idx)
                        .push(rows);
                    break 'main;
                } else if end.is_none() {
                    // cannot do better
                    // offsets.push(y);
                    dbg!();
                    builder = builder
                        .push(parent)
                        .push(idx1)
                        .push(bytes)
                        .push(no_s_idx)
                        .push(rows);
                    break 'main;
                } else {
                    // offsets.push(y);
                    x = child_id;
                    break;
                }
                offset += len;
            }
            use building::ReceiveRows;
            use top_down::ReceiveIdxNoSpace;
            use top_down::ReceiveOffset;
            builder = builder
                .push(parent)
                .push(idx1)
                .push(bytes)
                .push(no_s_idx)
                .push(rows);
        }

        let n = stores.resolve(&x);
        use building::SetLineSpan;
        use top_down::SetNode;
        if let Some(len) = n.try_bytes_len() {
            let lines = n.line_count();
            let node = x;
            builder.set(len).set(lines).set_node(node)
        } else {
            todo!()
        }
    }
}
