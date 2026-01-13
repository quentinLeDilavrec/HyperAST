#![allow(unused)] // WIP
//! inspired by the implementation in gumtree
//! further modularizations
//! TODO split IdD in 2 to help typecheck ids

use std::collections::HashSet;
use std::fmt::{Debug, Display};
use std::hash::Hash;

use hyperast::PrimInt;
use num_traits::{ToPrimitive, cast};

use hyperast::types::{HyperAST, HyperASTShared, HyperType, Labeled};

use super::Actions;
use super::action_vec::ActionsVec;

use crate::actions::script_generator2::{Act, ApplicablePath, SimpleAction};
use crate::decompressed_tree_store::{
    BreadthFirstIterable, DecompressedTreeStore, DecompressedWithParent, PostOrder,
    PostOrderIterable,
};
use crate::matchers::Mapping;
use crate::matchers::mapping_store::MonoMappingStore;
use crate::tree::tree_path::TreePath;
use crate::utils::sequence_algorithms::longest_common_subsequence;

struct InOrderNodes<IdD>(Option<Vec<IdD>>, HashSet<IdD>);

/// FEATURE: share parents
static COMPRESSION: bool = false;
static SUBTREE_DEL: bool = true;

type EditScriptError = String;

struct MidNode<IdC, IdD> {
    parent: IdD,
    compressed: IdC,
    children: Option<Vec<IdD>>,
    action: Option<usize>,
}

impl<IdC: Debug, IdD: Debug> Debug for MidNode<IdC, IdD> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("MidNode")
            .field("parent", &self.parent)
            .field("compressed", &self.compressed)
            .field("children", &self.children)
            .finish()
    }
}

pub struct ScriptGenerator<'a1, 'a2, IdM, SS, SD, HAST, M, P>
where
    HAST: HyperASTShared,
    M: MonoMappingStore,
{
    pub store: HAST,
    src_arena_dont_use: &'a1 SS,
    cpy2ori: Vec<M::Src>,
    #[allow(unused)]
    mid_arena: Vec<MidNode<HAST::IdN, IdM>>,
    mid_root: Vec<IdM>,
    dst_arena: &'a2 SD,
    cpy_mappings: M,
    dirty: bitvec::vec::BitVec,
    pub actions: ActionsVec<SimpleAction<HAST::Label, P, HAST::IdN>>,

    src_in_order: InOrderNodes<M::Src>,
    dst_in_order: InOrderNodes<M::Dst>,
}

impl<
    IdD: PrimInt + Debug + Hash + PartialEq + Eq,
    SS: DecompressedTreeStore<HAST, IdD>
        + DecompressedWithParent<HAST, IdD>
        + PostOrder<HAST, IdD>
        + PostOrderIterable<HAST, IdD>
        + Debug,
    SD: DecompressedTreeStore<HAST, IdD>
        + DecompressedWithParent<HAST, IdD>
        + BreadthFirstIterable<HAST, IdD>,
    HAST: HyperAST + Copy,
    M: MonoMappingStore<Src = IdD, Dst = IdD> + Default + Clone,
    P: TreePath<Item = HAST::Idx>,
> ScriptGenerator<'_, '_, IdD, SS, SD, HAST, M, P>
where
    HAST::Label: Debug + Eq + Copy,
    HAST::IdN: Debug + Clone,
    P: From<Vec<HAST::Idx>> + Debug,
{
    pub fn compute_actions(
        hast: HAST,
        mapping: &Mapping<SS, SD, M>,
    ) -> Result<ActionsVec<SimpleAction<HAST::Label, P, HAST::IdN>>, String> {
        ScriptGenerator::new(hast, &mapping.src_arena, &mapping.dst_arena)
            .init_cpy(&mapping.mappings)
            .generate()
            .map(|x| x.actions)
    }
}

impl<'a1, 'a2, IdD, SS, SD, HAST, M, P> ScriptGenerator<'a1, 'a2, IdD, SS, SD, HAST, M, P>
where
    HAST::Label: Debug + Eq + Copy,
    HAST::IdN: Debug,
    P: From<Vec<HAST::Idx>> + Debug,
    IdD: PrimInt + Debug + Hash + PartialEq + Eq,
    HAST: HyperAST + Copy,
    M: MonoMappingStore<Src = IdD, Dst = IdD> + Default + Clone,
    P: TreePath<Item = HAST::Idx>,
{
    pub fn new(store: HAST, src_arena: &'a1 SS, dst_arena: &'a2 SD) -> Self {
        Self {
            store,
            src_arena_dont_use: src_arena,
            cpy2ori: vec![],
            mid_arena: vec![],
            mid_root: vec![],
            dst_arena,
            cpy_mappings: Default::default(),
            dirty: Default::default(),
            actions: ActionsVec::new(),
            src_in_order: InOrderNodes(None, Default::default()),
            dst_in_order: InOrderNodes(None, Default::default()),
        }
    }
}
impl<
    'm,
    IdD: PrimInt + Debug + Hash + PartialEq + Eq,
    SS: DecompressedTreeStore<HAST, IdD>
        + DecompressedWithParent<HAST, IdD>
        + PostOrder<HAST, IdD>
        + PostOrderIterable<HAST, IdD>
        + Debug,
    SD: DecompressedTreeStore<HAST, IdD>
        + DecompressedWithParent<HAST, IdD>
        + BreadthFirstIterable<HAST, IdD>,
    HAST: HyperAST + Copy,
    M: MonoMappingStore<Src = IdD, Dst = IdD> + Default + Clone,
    P: TreePath<Item = HAST::Idx>,
> ScriptGenerator<'_, '_, IdD, SS, SD, HAST, M, P>
where
    HAST::Label: Debug + Eq + Copy,
    HAST::IdN: Debug,
    P: From<Vec<HAST::Idx>> + Debug,
{
    pub fn init_cpy(mut self, ms: &M) -> Self {
        // copy mapping
        self.cpy_mappings = ms.clone();
        let root = self.src_arena_dont_use.root();
        for x in self.src_arena_dont_use.iter_df_post::<true>() {
            let children = self.src_arena_dont_use.children(&x);
            let children = if !children.is_empty() {
                Some(children)
            } else {
                None
            };
            self.mid_arena.push(MidNode {
                parent: self.src_arena_dont_use.parent(&x).unwrap_or(root),
                compressed: self.src_arena_dont_use.original(&x),
                children,
                action: None,
            });
            self.dirty.push(false);
        }
        self.mid_root = vec![root];
        self
    }
}

impl<
    IdD: PrimInt + Debug + Hash + PartialEq + Eq,
    SS: DecompressedTreeStore<HAST, IdD>
        + DecompressedWithParent<HAST, IdD>
        + PostOrder<HAST, IdD>
        + PostOrderIterable<HAST, IdD>
        + Debug,
    SD: DecompressedTreeStore<HAST, IdD>
        + DecompressedWithParent<HAST, IdD>
        + BreadthFirstIterable<HAST, IdD>,
    HAST: HyperAST + Copy,
    M: MonoMappingStore<Src = IdD, Dst = IdD> + Default + Clone,
    P: TreePath<Item = HAST::Idx>,
> ScriptGenerator<'_, '_, IdD, SS, SD, HAST, M, P>
where
    HAST::Label: Debug + Eq + Copy,
    HAST::IdN: Debug,
    P: From<Vec<HAST::Idx>> + Debug,
{
    pub fn _compute_actions(
        store: HAST,
        src_arena: &SS,
        dst_arena: &SD,
        ms: &M,
    ) -> Result<ActionsVec<SimpleAction<HAST::Label, P, HAST::IdN>>, String> {
        ScriptGenerator::new(store, src_arena, dst_arena)
            .init_cpy(ms)
            .generate()
            .map(|x| x.actions)
    }

    pub fn precompute_actions<'a>(
        store: HAST,
        src_arena: &'a SS,
        dst_arena: &'a SD,
        ms: &'a M,
    ) -> ScriptGenerator<'a, 'a, IdD, SS, SD, HAST, M, P> {
        ScriptGenerator::new(store, src_arena, dst_arena).init_cpy(ms)
    }

    pub fn generate(mut self) -> Result<Self, String> {
        self.ins_mov_upd()?;
        self.del()?;
        Ok(self)
    }

    fn ins_mov_upd(&mut self) -> Result<(), EditScriptError> {
        if COMPRESSION {
            todo!()
        }
        self.auxiliary_ins_mov_upd(&|_, _| ())
    }

    pub fn auxiliary_ins_mov_upd(
        &mut self,
        f: &impl Fn(&HAST::IdN, &HAST::IdN),
    ) -> Result<(), EditScriptError> {
        for x in self.dst_arena.iter_bf() {
            let y = self.dst_arena.parent(&x);
            let z = y.map(|y| self.cpy_mappings.get_src_unchecked(&y));
            let w = if !self.cpy_mappings.is_dst(&x) {
                self.ins_aux(x, y, z)?
            } else {
                self.mov_upd_aux(f, x, y, z)?
            };
            self.src_in_order.push(w);
            self.dst_in_order.push(x);
            self.align_children(&w, &x);
        }
        Ok(())
    }

    pub fn ins_aux(&mut self, x: IdD, y: Option<IdD>, z: Option<IdD>) -> Result<IdD, String> {
        let k = y.map(|y| self.find_pos(&x, &y));
        let w = self.make_inserted_node(&x, &z);
        let ori = self.path_dst(&self.dst_arena.root(), &x);
        let mid = if let Some(z) = z {
            self.path(z).extend(&[k.unwrap()])
        } else {
            let k = k.unwrap_or(num_traits::one());
            vec![k].into()
        };
        let path = ApplicablePath { ori, mid };
        let action = Act::Insert {
            sub: self.dst_arena.original(&x),
        };
        let action = SimpleAction { path, action };
        let Some(z) = z else {
            self.mid_root.push(w);
            self.mid_arena[w.index()].action = Some(self.actions.len());
            self.actions.push(action);
            return Ok(w);
        };
        if let Some(cs) = self.mid_arena[z.index()].children.as_mut() {
            cs.insert(k.unwrap().index(), w);
        } else {
            self.mid_arena[z.index()].children = Some(vec![w])
        }
        self.mid_arena[w.index()].action = Some(self.actions.len());
        self.actions.push(action);
        Ok(w)
    }

    pub fn mov_upd_aux(
        &mut self,
        f: &impl Fn(&HAST::IdN, &HAST::IdN),
        x: IdD,
        y: Option<IdD>,
        z: Option<IdD>,
    ) -> Result<IdD, String> {
        let w = self.cpy_mappings.get_src_unchecked(&x);
        let v = self.mid_arena[w.index()].parent;
        let v = if v == w { None } else { Some(v) };

        let w_t = &self.mid_arena[w.index()].compressed;
        let x_t = &self.dst_arena.original(&x);
        f(&w_t, &x_t);
        let w_l = self.store.resolve(w_t).try_get_label().cloned();
        let x_l = self.store.resolve(x_t).try_get_label().cloned();

        if z != v {
            self.mov_aux(x, y, z, w, v, w_l, x_l);
        } else if w_l != x_l {
            self.upd_aux(x, w, x_l);
        }
        self.md_for_middle(&x, &w);
        Ok(w)
    }

    pub fn mov_aux(
        &mut self,
        x: IdD,
        y: Option<IdD>,
        z: Option<IdD>,
        w: IdD,
        v: Option<IdD>,
        w_l: Option<HAST::Label>,
        x_l: Option<HAST::Label>,
    ) -> Result<(), EditScriptError> {
        let from = ApplicablePath {
            ori: self.orig_src(w),
            mid: self.path(w),
        };
        // remove moved node
        // TODO do not mutate existing node
        if let Some(v) = v {
            let _v: &mut MidNode<HAST::IdN, IdD> = &mut self.mid_arena[v.index()];
            let cs = _v.children.as_mut().unwrap();
            let idx = cs.iter().position(|x| x == &w).unwrap();
            cs.remove(idx);
            self.dirty.set(v.index(), true);
        }

        let k = if let Some(y) = y {
            self.find_pos(&x, &y)
        } else {
            num_traits::zero()
        };
        let mid = if let Some(z) = z {
            self.path(z).extend(&[k])
        } else {
            vec![k].into()
        };
        let ori = self.path_dst(&self.dst_arena.root(), &x);

        let act = if w_l != x_l {
            let mid = if let Some(z) = z {
                self.path(z).extend(&[k])
            } else {
                vec![k].into()
            };
            let ori = self.orig_src(w);
            let before = ApplicablePath { ori, mid };
            let ori = self.path_dst(&self.dst_arena.root(), &x);
            let mid = if let Some(z) = z {
                self.path(z).extend(&[k])
            } else {
                vec![k].into()
            };
            let path = ApplicablePath { ori, mid };
            let x_l =
                x_l.expect("no label on x, a given node type either always or never has a label");
            let action = self._upd(x, w, x_l, before, path)?;
            self.actions.push(action);
            Act::Move { from }
        } else {
            Act::Move { from }
        };
        // TODO do not mutate existing node
        if let Some(z) = z {
            if let Some(cs) = self.mid_arena[z.index()].children.as_mut() {
                cs.insert(k.index(), w)
            } else {
                self.mid_arena[z.index()].children = Some(vec![w])
            };
            self.mid_arena[w.index()].parent = z;
        } else {
            self.mid_arena[w.index()].parent = w;
        }
        self.mid_arena[w.index()].action = Some(self.actions.len());
        if let Act::MovUpd { .. } = act {
            self.mid_arena[w.index()].compressed = self.dst_arena.original(&x);
        }
        let path = ApplicablePath { ori, mid };
        let action = SimpleAction { path, action: act };
        self.actions.push(action);
        Ok(())
    }

    pub fn upd_aux(
        &mut self,
        x: IdD,
        w: IdD,
        x_l: Option<HAST::Label>,
    ) -> Result<(), EditScriptError> {
        let before = ApplicablePath {
            ori: self.orig_src(w),
            mid: self.path(w),
        };
        let path = ApplicablePath {
            ori: self.path_dst(&self.dst_arena.root(), &x),
            mid: self.path(w),
        };
        let x_l = x_l.expect("no label on x, a given node type either always or never has a label");
        let action = self._upd(x, w, x_l, before, path)?;
        self.actions.push(action);
        Ok(())
    }

    pub fn _upd(
        &mut self,
        x: IdD,
        w: IdD,
        x_l: HAST::Label,
        before: ApplicablePath<P>,
        path: ApplicablePath<P>,
    ) -> Result<SimpleAction<HAST::Label, P, HAST::IdN>, EditScriptError> {
        let action = Act::Update { new: x_l, before };
        let action = SimpleAction { path, action };
        assert!(self.cpy_mappings.is_src(&w));
        assert!(self.cpy_mappings.is_dst(&x));
        self.mid_arena[w.index()].compressed = self.dst_arena.original(&x);
        self.mid_arena[w.index()].action = Some(self.actions.len());
        Ok(action)
    }

    pub fn del(&mut self) -> Result<(), EditScriptError> {
        let root = *self.mid_root.first().unwrap();
        let parent: Ele<IdD, usize, _> = Ele::new(root);
        let mut parent = vec![parent];
        loop {
            let next;
            let waiting;
            loop {
                let Some(ele) = parent.pop() else {
                    next = None;
                    waiting = vec![];
                    break;
                };
                let curr = &self.mid_arena[ele.id.index()];
                let Some(cs) = &curr.children else {
                    next = Some(ele.id);
                    waiting = ele.w;
                    break;
                };
                if cs.len() == ele.idx {
                    next = Some(ele.id);
                    waiting = ele.w;
                    break;
                }
                let child = cs[ele.idx];
                parent.push(ele.inc());
                parent.push(Ele::new(child));
            }
            let Some(w) = next else {
                break;
            };
            if self.dirty[w.index()] {
                // dbg!(w);
            }
            if !self.cpy_mappings.is_src(&w) {
                let action = self._del(&mut parent, w)?;
                if SUBTREE_DEL {
                    if self.dirty[w.index()] {
                        // non uniform del.
                        self.actions.extend(waiting);
                        log::trace!("{:?}", action);
                        self.actions.push(action);
                        let _w: &mut MidNode<HAST::IdN, IdD> = &mut self.mid_arena[w.index()];
                        let v = _w.parent;
                        // transitively
                        self.dirty.set(v.index(), true);
                    } else if let Some(i) = parent.len().checked_sub(1) {
                        // uniform, so wait in parent
                        parent[i].w.push(action);
                    } else {
                        log::trace!("{:?}", action);
                        self.actions.push(action);
                    }
                } else {
                    log::trace!("{:?}", action);
                    self.actions.push(action);
                }
            } else {
                if SUBTREE_DEL {
                    self.actions.extend(waiting);
                }
            }
        }
        Ok(())
    }

    fn _del(
        &mut self,
        parent: &mut Vec<Ele<IdD, usize, SimpleAction<HAST::Label, P, HAST::IdN>>>,
        w: IdD,
    ) -> Result<SimpleAction<HAST::Label, P, HAST::IdN>, EditScriptError> {
        // TODO mutate mid arena ?
        let path = ApplicablePath {
            ori: self.orig_src(w),
            mid: self.path(w),
        };
        let _w = &mut self.mid_arena[w.index()];
        let v = _w.parent;
        let _v = &mut self.mid_arena[v.index()];
        if v != w {
            let cs = _v.children.as_mut().unwrap();
            let idx = cs.iter().position(|x| x == &w).unwrap();
            cs.remove(idx);
            let Some(parent) = parent.last_mut() else {
                unreachable!("there should be at least the fake root")
            };
            assert!(parent.idx > 0);
            parent.idx -= 1;
        }
        _v.action = Some(self.actions.len());
        let action = Act::Delete {};
        let action = SimpleAction { path, action };
        Ok(action)
    }

    pub(crate) fn align_children(&mut self, w: &IdD, x: &IdD) {
        let d = vec![];
        let w_c = self.mid_arena[w.index()].children.as_ref().unwrap_or(&d);
        self.src_in_order.remove_all(w_c);
        let x_c = self.dst_arena.children(x);
        self.dst_in_order.remove_all(x_c.as_slice());

        let mut s1 = w_c
            .iter()
            .filter(|c| self.cpy_mappings.is_src(c))
            .filter(|c| x_c.contains(&self.cpy_mappings.get_dst_unchecked(c)))
            .copied()
            .collect::<Vec<_>>();

        let mut s2 = w_c
            .iter()
            .filter(|c| self.cpy_mappings.is_dst(c))
            .filter(|c| w_c.contains(&self.cpy_mappings.get_src_unchecked(c)))
            .copied()
            .collect::<Vec<_>>();

        let lcs = self.lcs(&s1, &s2);

        for m in &lcs {
            self.src_in_order.push(m.0);
            self.dst_in_order.push(m.1);
        }

        let z = *w;
        for a in &s1 {
            for b in &s2 {
                if self.cpy_mappings.has(a, b) && !lcs.contains(&(*a, *b)) {
                    self.align_children_aux(x, z, a, b);
                    self.src_in_order.push(*a);
                    self.dst_in_order.push(*b);
                }
            }
        }
    }

    pub(crate) fn align_children_aux(&mut self, x: &IdD, z: IdD, a: &IdD, b: &IdD) {
        let k = self.find_pos(b, x);
        let path = ApplicablePath {
            ori: self.orig_src(z).extend(&[k]),
            mid: self.path(z),
        };
        let action = Act::Move {
            from: ApplicablePath {
                ori: self.orig_src(*a),
                mid: self.path(*a),
            },
        };
        let action = SimpleAction { path, action };
        let cs = self.mid_arena[z.index()].children.as_mut().unwrap();
        let idx = cs.iter().position(|x| x == a).unwrap();
        cs.remove(idx);
        if let Some(cs) = self.mid_arena[z.index()].children.as_mut() {
            let k = cast(k).unwrap();
            if k < cs.len() {
                cs.insert(k, *a)
            } else {
                cs.push(*a)
            }
        } else {
            self.mid_arena[z.index()].children = Some(vec![*a])
        };
        self.mid_arena[a.index()].parent = z;
        self.mid_arena[a.index()].action = Some(self.actions.len());

        self.actions.push(action);
    }

    fn orig_src(&self, v: IdD) -> P {
        self.src_arena_dont_use
            .path(&self.src_arena_dont_use.root(), &self.copy_to_orig(v))
            .into()
    }
}

impl<IdD, SS, SD, HAST, M, P> ScriptGenerator<'_, '_, IdD, SS, SD, HAST, M, P>
where
    P: From<Vec<HAST::Idx>> + Debug,
    IdD: PrimInt + Debug + Hash,
    M::Src: PrimInt + Debug + Hash,
    M::Dst: PrimInt + Debug + Hash,
    HAST: HyperAST + Copy,
    SS: DecompressedTreeStore<HAST, M::Src>,
    SD: DecompressedTreeStore<HAST, M::Dst>,
    SD: DecompressedWithParent<HAST, M::Dst>,
    M: MonoMappingStore<Src = IdD, Dst = IdD>,
{
    /// find position of x in parent on dst_arena
    pub(crate) fn find_pos(&self, x: &M::Dst, y: &M::Dst) -> HAST::Idx {
        let siblings = self.dst_arena.children(y);

        for c in &siblings {
            if self.dst_in_order.contains(c) {
                if c == x {
                    return num_traits::zero();
                } else {
                    break;
                }
            }
        }
        let xpos: usize = self.dst_arena.position_in_parent(x).unwrap();
        let mut v: Option<M::Dst> = None;
        for i in 0..xpos {
            let c: &M::Dst = &siblings[i];
            if self.dst_in_order.contains(c) {
                v = Some(*c);
            };
        }

        if v.is_none() {
            return num_traits::zero();
        }

        let u = self.cpy_mappings.get_src_unchecked(&v.unwrap());
        let upos: HAST::Idx = {
            let p = self.mid_arena[u.index()].parent;
            let cs = self.mid_arena[p.index()].children.as_ref().unwrap();
            let r = cs.iter().position(|y| y == &u).unwrap();
            cast(r).unwrap()
        };
        upos + num_traits::one()
    }

    pub(crate) fn lcs(
        &self,
        src_children: &[M::Src],
        dst_children: &[M::Dst],
    ) -> Vec<(M::Src, M::Dst)> {
        longest_common_subsequence(src_children, dst_children, |src, dst| {
            self.cpy_mappings.has(src, dst)
        })
        .into_iter()
        .map(|m: (M::Src, M::Dst)| (src_children[m.0.index()], dst_children[m.1.index()]))
        .collect()
    }

    pub(crate) fn md_for_middle(
        &self,
        _x: &M::Dst,
        // not sure there
        _w: &M::Dst,
    ) {
        // todo maybe later
    }

    pub(crate) fn make_inserted_node(&mut self, x: &M::Dst, z: &Option<M::Dst>) -> IdD {
        let w = cast(self.mid_arena.len()).unwrap();
        let z = if let Some(z) = z { *z } else { w };
        self.mid_arena.push(MidNode {
            parent: z,
            compressed: self.dst_arena.original(x),
            children: None,
            action: None,
        });
        // self.moved.push(false);
        self.dirty.push(false);
        let (src_to_dst_l, dst_to_src_l) = self.cpy_mappings.capacity();
        self.cpy_mappings.topit(src_to_dst_l, dst_to_src_l);
        self.cpy_mappings.link(w, *x);
        w
    }

    fn copy_to_orig(&self, w: M::Src) -> M::Src {
        if self.src_arena_dont_use.len() <= w.index() {
            return self.cpy2ori[w.index() - self.src_arena_dont_use.len()];
        }
        w
    }

    fn path_dst(&self, root: &M::Dst, x: &M::Dst) -> P {
        let mut r = vec![];
        let mut x = *x;
        loop {
            let p = self.dst_arena.parent(&x);
            if let Some(p) = p {
                r.push(self.dst_arena.position_in_parent(&x).unwrap());
                x = p
            } else {
                assert_eq!(root, &x);
                break;
            }
        }
        r.reverse();
        r.into()
    }

    fn path(&self, mut z: IdD) -> P {
        let mut r = vec![];
        loop {
            let p = self.mid_arena[z.index()].parent;
            if p == z {
                let i = self
                    .mid_root
                    .iter()
                    .position(|x| x == &z)
                    .expect("expect the position of z in children of mid_root");
                r.push(cast(i).unwrap());
                break;
            } else {
                let i = self.mid_arena[p.index()]
                    .children
                    .as_ref()
                    .expect(
                        "parent should have children, current node should actually be one of them",
                    )
                    .iter()
                    .position(|x| x == &z)
                    .expect("expect the position of z in children of p");
                r.push(cast(i).unwrap());
                z = p;
            }
        }
        r.reverse();
        r.into()
    }
}

struct Ele<IdD, Idx, W> {
    // id in arena
    id: IdD,
    // curent child offset
    idx: Idx,
    // true if not only deletes
    // TODO use a bitset to reduce mem.
    w: Vec<W>,
}

impl<IdD, Idx: num_traits::Zero, W> Ele<IdD, Idx, W> {
    fn new(id: IdD) -> Self {
        let idx = num_traits::zero();
        let _b = false;
        let w = vec![];
        Self { id, idx, w }
    }
}
impl<IdD, Idx: num_traits::PrimInt, W> Ele<IdD, Idx, W> {
    fn inc(mut self) -> Self {
        self.idx = self.idx + num_traits::one();
        self
    }
}

struct Iter<'a, IdC, IdD: PrimInt> {
    parent: Vec<(IdD, usize)>,
    mid_arena: &'a mut [MidNode<IdC, IdD>],
}

impl<IdC, IdD: PrimInt> Iterator for Iter<'_, IdC, IdD> {
    type Item = IdD;

    fn next(&mut self) -> Option<Self::Item> {
        loop {
            let (id, idx) = self.parent.pop()?;
            let curr = &self.mid_arena[id.index()];
            let Some(cs) = &curr.children else {
                return Some(id);
            };
            if cs.len() == idx {
                return Some(id);
            } else {
                self.parent.push((id, idx + 1));
                self.parent.push((cs[idx], 0));
            }
        }
    }
}

impl<IdD: Hash + Eq + Clone> InOrderNodes<IdD> {
    /// TODO add precondition to try to linearly remove element (if both ordered the same way it's easy to remove without looking at lists multiple times)
    /// Maybe use a bloom filter with a collision set ? do we have a good estimate of the number of element to store ?
    fn remove_all(&mut self, w: &[IdD]) {
        w.iter().for_each(|x| {
            self.1.remove(x);
        });
    }

    pub(crate) fn push(&mut self, x: IdD) {
        self.1.insert(x.clone());
    }

    fn contains(&self, x: &IdD) -> bool {
        self.1.contains(x)
    }
}
