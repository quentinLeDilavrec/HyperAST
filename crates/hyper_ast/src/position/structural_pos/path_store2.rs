use super::*;

impl<IdN, Idx> Debug for StructuralPositionStore2<IdN, Idx> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("StructuralPositionStore")
            .field("persisted", &self.persisted)
            .field("nodes", &self.nodes.len())
            .field("parents", &self.parents.len())
            .field("offsets", &self.offsets.len())
            .finish()
    }
}

impl<IdN, Idx> StructuralPositionStore2<IdN, Idx> {
    pub fn persist(&mut self, h: Handle) {
        if self.persisted.0 < h.0 {
            self.persisted.0 = h.0;
        }
    }
    pub fn parent(&self, h: Handle) -> Option<Handle> {
        if h.0 == 0 {
            panic!();
        }
        if h.0 == 1 {
            return None;
        }
        assert!(self.parents[h.0 - 1].0 < h.0);
        Some(self.parents[h.0 - 1])
    }
    pub fn node(&self, h: Handle) -> IdN
    where
        IdN: Copy,
    {
        if h.0 == 0 {
            panic!();
        }
        self.nodes[h.0 - 1]
    }
    pub fn offset(&self, h: Handle) -> Idx
    where
        Idx: Copy,
    {
        if h.0 == 0 {
            panic!();
        }
        self.offsets[h.0 - 1]
    }

    pub fn inc(&mut self, h: Handle, node: IdN) -> Handle
    where
        Idx: PrimInt,
    {
        if h.0 == 0 {
            panic!();
        }
        if self.persisted.0 < h.0 {
            self.nodes[h.0 - 1] = node;
            self.offsets[h.0 - 1] += num::one();
            return h;
        }
        let Some(p) = self.parent(h) else {
            unreachable!()
        };
        if self.persisted.0 == self.nodes.len() {
            self.nodes.push(node);
            self.offsets.push(self.offsets[h.0 - 1] + num::one());
            self.parents.push(p);
            let mut h = self.persisted;
            h.0 += 1;
            h
        } else {
            assert!(self.nodes.len() > self.persisted.0);
            self.nodes[self.persisted.0] = node;
            self.offsets[self.persisted.0] = self.offsets[h.0 - 1] + num::one();
            self.parents[self.persisted.0] = p;
            let mut h = self.persisted;
            h.0 += 1;
            h
        }
    }

    pub fn down(&mut self, h: Handle, node: IdN, offset: Idx) -> Handle {
        if self.persisted.0 > h.0 {
            let mut h = h;
            h.0 -= 1;
            self.nodes[self.persisted.0] = node;
            self.offsets[self.persisted.0] = offset;
            self.parents[self.persisted.0] = h;
            let mut r = self.persisted;
            r.0 += 1;
            return r;
        }
        let mut c = h;
        c.0 += 1;
        if self.nodes.len() == c.0 - 1 {
            assert_eq!(self.offsets.len(), c.0 - 1);
            self.nodes.push(node);
            self.offsets.push(offset);
            self.parents.push(h);
        } else if self.nodes.len() < c.0 - 1 {
            dbg!(self.nodes.len());
            dbg!(self.offsets.len());
            dbg!(self.persisted.0);
            dbg!(c.0);
            panic!()
        } else {
            self.nodes[c.0 - 1] = node;
            self.offsets[c.0 - 1] = offset;
            self.parents[c.0 - 1] = h;
        }
        c
    }
}
