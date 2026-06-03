use std::fmt::{Debug, Display};

use num_traits::{PrimInt, ToPrimitive, Zero};

use hyperast::types::HyperAST;
use hyperast::types::WithChildren as _;
use hyperast::types::WithSerialization;
use hyperast::types::{LabelStore, Labeled, NodeStore};

use crate::decompressed_tree_store::PostOrder;

pub struct SimplePreOrderMapper<'a, IdD, D> {
    pub map: Vec<IdD>,
    rev: Vec<IdD>,
    pub(crate) depth: Vec<u16>,
    back: &'a D,
}

impl<IdD: Debug, D: Debug> Debug for SimplePreOrderMapper<'_, IdD, D> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("SD")
            .field("map", &self.map)
            .field("rev", &self.rev)
            .field("back", &self.back)
            .finish()
    }
}

impl<'a, IdD: PrimInt, D> From<&'a D> for SimplePreOrderMapper<'a, IdD, D> {
    fn from(_x: &'a D) -> Self {
        todo!()
        // look at previous attempts in the history
    }
}

pub struct DisplaySimplePreOrderMapper<'store: 'a, 'a: 'b, 'b, IdD: PrimInt, HAST: HyperAST, D> {
    pub inner: &'b SimplePreOrderMapper<'a, IdD, D>,
    pub stores: &'store HAST,
}

impl<'store: 'a, 'a: 'b, 'b, IdD: PrimInt, HAST, D> Display
    for DisplaySimplePreOrderMapper<'store, 'a, 'b, IdD, HAST, D>
where
    HAST: HyperAST + Copy,
    for<'t> hyperast::types::LendT<'t, HAST>: WithSerialization,
    D: PostOrder<HAST, IdD, IdD = IdD>,
{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut pos = 0;
        for i in 0..self.inner.map.len() {
            let o = self.inner.map[i];
            let ori = self.inner.back.original(&o);
            let node = self.stores.node_store().resolve(&ori);
            let len = node.try_bytes_len().unwrap_or(0);
            writeln!(
                f,
                "{:>3}:{} {:?}    [{},{}]",
                o.to_usize().unwrap(),
                "  ".repeat(self.inner.depth[i].to_usize().unwrap()),
                self.stores.resolve_type(&ori),
                pos,
                pos + len,
            )?;
            if node.child_count().is_zero() {
                pos += len;
            }
        }
        Ok(())
    }
}
impl<'store: 'a, 'a: 'b, 'b, IdD: PrimInt, HAST, D> Debug
    for DisplaySimplePreOrderMapper<'store, 'a, 'b, IdD, HAST, D>
where
    HAST: HyperAST + Copy,
    D: PostOrder<HAST, IdD, IdD = IdD>,
{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if f.alternate() {
            for i in 0..self.inner.map.len() {
                let o = self.inner.map[i];
                let ori = self.inner.back.original(&o);
                let node = self.stores.node_store().resolve(&ori);
                let mut s = self
                    .stores
                    .label_store()
                    .resolve(node.get_label_unchecked())
                    .to_owned();
                s.truncate(5);
                writeln!(
                    f,
                    "{:>3}:{} {:?}; {}",
                    o.to_usize().unwrap(),
                    "  ".repeat(self.inner.depth[i].to_usize().unwrap()),
                    self.stores.resolve_type(&ori),
                    s.escape_debug()
                )?;
            }
            return Ok(());
        }
        for i in 0..self.inner.map.len() {
            let o = self.inner.map[i];
            let ori = self.inner.back.original(&o);
            writeln!(
                f,
                "{:>3}:{} {:?}",
                o.to_usize().unwrap(),
                "  ".repeat(self.inner.depth[i].to_usize().unwrap()),
                self.stores.resolve_type(&ori),
            )?;
        }
        Ok(())
    }
}
