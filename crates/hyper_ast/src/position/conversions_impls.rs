use num::{one, zero};
use std::fmt::Debug;

use crate::types::{Children, WithChildren};
use crate::types::{HyperAST, LabelStore, NodeStore};
use crate::types::{HyperType, Labeled, LendT, NodeId};
use crate::types::{WithSerialization, WithStats};

use super::building;
use super::building::{bottom_up, top_down};
use super::{WithHyperAstPositionConverter, position_accessors};

impl<HAST, S> WithHyperAstPositionConverter<'_, '_, S, HAST>
where
    S: super::node_filter_traits::Full,
    HAST: HyperAST,
    HAST::IdN: NodeId<IdN = HAST::IdN> + Eq + Debug + Copy,
    HAST::Idx: Debug,
{
    pub fn compute_pos_post_order<O, B>(&self) -> O
    where
        S: position_accessors::WithFullPostOrderPath<HAST::IdN, Idx = HAST::Idx>
            + position_accessors::SolvedPosition<HAST::IdN>,
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

    pub fn compute_pos_pre_order<O, B>(&self) -> O
    where
        S: position_accessors::WithPreOrderOffsets<Idx = HAST::Idx>
            + position_accessors::RootedPosition<HAST::IdN>,
        for<'t> LendT<'t, HAST>: WithSerialization + WithStats,
        B: top_down::ReceiveDir<HAST::IdN, HAST::Idx, O> + top_down::CreateBuilder,
        B::SB1<O>: top_down::ReceiveInFile<HAST::IdN, HAST::Idx, usize, O>,
    {
        let mut builder: B = top_down::CreateBuilder::create();
        let stores = self.stores;
        let mut x = self.src.root();
        let mut it = self.src.iter_offsets();

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
            dbg!(o);
            let n = stores.resolve(&x);
            let cs = n.children().unwrap();
            let c = cs.get(o).unwrap();
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
