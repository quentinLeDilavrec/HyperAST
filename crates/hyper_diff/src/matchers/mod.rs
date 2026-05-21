//! Matchers associate nodes in pairs of tree.
//!
//! Originally, the matching was a phase in a tree-diff algorithm,
//! where interpreting the matchings (or mappings) would allow to produce a set of actions to transform a given tree into another.
//! In this context, the objective is to minimize the transformation cost, e.g., the number and types of actions.
//!
//! Later the notion of matchings was extended,
//! leading to many different matching approaches.
//! Certain matching approaches also consider more semantic interpretations.
//! Moreover, matchers can also be composed.
//!
//! [`Decompressible`], [`Mapper`], and [`Mapping`] are the core types
//! structuring the matching process, allowing to switch between different memory layouts
//! required by different matching approaches.
//! See [`crate::decompressed_tree_store`] and [`crate::mappings`] for the available layouts.
//!
//! As a side note, relative to non-trivial decision involving the HyperAST.
//! While matching trees, you should most likely not require an owned or mutably referenced HyperAST.
//! More specifically, when seeing `HAST: HyperAST + Copy` the `Copy` should preferably be an immutable reference to an HyperAST.
//! Consequently, [`Decompressible`] and [`Mapper`] should be short lived, dropped before going back adding more code to the HyperAST.
//! In the case, you need longer-lived handling of mappings, only keep [`Mapping`] alive, then later re-associate it with the same HyperAST.
//!
//! Caution: Directly manipulating [`Mapping`] while holding multiple HyperASTs could lead to incoherent results,
//! if the same [`Mapping`] is reused with a different HyperAST.

pub mod heuristic;
pub mod optimal;

#[cfg(test)]
mod tests;

use std::ops::{Deref, DerefMut};

use hyperast::types::{DecompressedFrom, HyperAST, HyperASTShared};

use crate::mappings::{MappingStore, VecStore};

pub struct Decompressible<HAST, D> {
    /// the HyperAST which is being decompressed
    pub hyperast: HAST,
    /// the structure containing the (partially) decompressed subtree
    pub decomp: D,
}

impl<HAST, D: std::fmt::Debug> std::fmt::Debug for Decompressible<HAST, D> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("Decompressible")
            // .field("hyperast", &self.hyperast)
            .field("decomp", &self.decomp)
            .finish()
    }
}

impl<HAST: HyperAST + Copy, D: DecompressedFrom<HAST>> DecompressedFrom<HAST>
    for Decompressible<HAST, D>
{
    type Out = Decompressible<HAST, D::Out>;

    fn decompress(hyperast: HAST, id: &HAST::IdN) -> Self::Out {
        Decompressible {
            hyperast,
            decomp: D::decompress(hyperast, id),
        }
    }
}

impl<HAST, D> std::ops::Deref for Decompressible<HAST, D> {
    type Target = D;
    fn deref(&self) -> &Self::Target {
        &self.decomp
    }
}

impl<HAST, D> Decompressible<HAST, D> {
    pub fn map<DD>(self, f: impl Fn(D) -> DD) -> Decompressible<HAST, DD> {
        Decompressible {
            hyperast: self.hyperast,
            decomp: f(self.decomp),
        }
    }
}

impl<HAST, D> std::ops::DerefMut for Decompressible<HAST, D> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.decomp
    }
}

impl<HAST: Copy, D: Clone> Clone for Decompressible<HAST, D> {
    fn clone(&self) -> Self {
        Self {
            hyperast: self.hyperast,
            decomp: self.decomp.clone(),
        }
    }
}

impl<HAST: Copy, D> Decompressible<HAST, D> {
    pub fn as_ref(&self) -> Decompressible<HAST, &D> {
        Decompressible {
            hyperast: self.hyperast,
            decomp: &self.decomp,
        }
    }
    pub fn as_mut(&mut self) -> Decompressible<HAST, &mut D> {
        Decompressible {
            hyperast: self.hyperast,
            decomp: &mut self.decomp,
        }
    }
}

pub trait WithMappings {
    type M;
}

/// Convenience wrapper enabling the computation of mappings between two trees stored in a [`HyperAST`].
#[derive(Clone)]
pub struct Mapper<HAST, Dsrc, Ddst, M> {
    /// the hyperAST to whom mappings are coming
    pub hyperast: HAST,
    /// the decompressed subtrees coming from hyperAST and their mappings
    pub mapping: Mapping<Dsrc, Ddst, M>,
}

// TODO provide an helper to safely "disconnect and reconnect" to the same HyperAST

impl<HAST, Dsrc, Ddst, M> WithMappings for Mapper<HAST, Dsrc, Ddst, M> {
    type M = M;
}

impl<HAST: Copy, Dsrc, Ddst, M>
    Mapper<HAST, Decompressible<HAST, Dsrc>, Decompressible<HAST, Ddst>, M>
{
    pub fn mut_decompressible(
        &mut self,
    ) -> Mapper<HAST, Decompressible<HAST, &mut Dsrc>, Decompressible<HAST, &mut Ddst>, &mut M>
    {
        let hyperast = self.hyperast;
        let mapping = &mut self.mapping;
        Mapper {
            hyperast,
            mapping: Mapping {
                src_arena: Decompressible {
                    hyperast,
                    decomp: &mut mapping.src_arena,
                },
                dst_arena: Decompressible {
                    hyperast,
                    decomp: &mut mapping.dst_arena,
                },
                mappings: &mut mapping.mappings,
            },
        }
    }
}

impl<HAST: Copy, Dsrc, Ddst, M> Mapper<HAST, Dsrc, Ddst, M> {
    pub fn split_mut(
        &mut self,
    ) -> Mapper<HAST, Decompressible<HAST, &mut Dsrc>, Decompressible<HAST, &mut Ddst>, &mut M>
    {
        let hyperast = self.hyperast;
        let mapping = &mut self.mapping;
        Mapper {
            hyperast,
            mapping: Mapping {
                src_arena: Decompressible {
                    hyperast,
                    decomp: &mut mapping.src_arena,
                },
                dst_arena: Decompressible {
                    hyperast,
                    decomp: &mut mapping.dst_arena,
                },
                mappings: &mut mapping.mappings,
            },
        }
    }

    pub fn with_mut_decompressible(
        owned: &mut (Decompressible<HAST, Dsrc>, Decompressible<HAST, Ddst>),
        mappings: M,
    ) -> Mapper<HAST, Decompressible<HAST, &mut Dsrc>, Decompressible<HAST, &mut Ddst>, M>
    where
        M: Default,
    {
        Mapper {
            hyperast: owned.0.hyperast,
            mapping: crate::matchers::Mapping {
                src_arena: owned.0.as_mut(),
                dst_arena: owned.1.as_mut(),
                mappings,
            },
        }
    }

    pub fn new(hyperast: HAST, mappings: M, owned: (Dsrc, Ddst)) -> Mapper<HAST, Dsrc, Ddst, M> {
        Mapper {
            hyperast,
            mapping: crate::matchers::Mapping {
                src_arena: owned.0,
                dst_arena: owned.1,
                mappings,
            },
        }
    }

    pub fn prep(
        hyperast: HAST,
        mappings: M,
        owned: (Dsrc, Ddst),
    ) -> Mapper<HAST, Decompressible<HAST, Dsrc>, Decompressible<HAST, Ddst>, M> {
        Mapper {
            hyperast,
            mapping: crate::matchers::Mapping {
                src_arena: Decompressible {
                    decomp: owned.0,
                    hyperast,
                },
                dst_arena: Decompressible {
                    decomp: owned.1,
                    hyperast,
                },
                mappings,
            },
        }
    }
}
impl<HAST: Copy, Dsrc, Ddst, IdD> Mapper<HAST, Dsrc, Ddst, VecStore<IdD>> {
    pub fn mirror(self) -> Mapper<HAST, Ddst, Dsrc, VecStore<IdD>> {
        Mapper {
            hyperast: self.hyperast,
            mapping: Mapping {
                src_arena: self.mapping.dst_arena,
                dst_arena: self.mapping.src_arena,
                mappings: self.mapping.mappings.mirror(),
            },
        }
    }
}

// NOTE this is temporary, waiting for the refactoring of helpers
// the refactoring is simple, do a spliting borrow, before accessing content
// TODO remove these deref impls
impl<HAST, Dsrc, Ddst, M> Deref for Mapper<HAST, Dsrc, Ddst, M> {
    type Target = Mapping<Dsrc, Ddst, M>;

    fn deref(&self) -> &Self::Target {
        &self.mapping
    }
}

impl<HAST, Dsrc, Ddst, M> DerefMut for Mapper<HAST, Dsrc, Ddst, M> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.mapping
    }
}

#[derive(Clone)]
/// A mapping between two trees, containing mappings between node pairs (or tuples).
pub struct Mapping<Dsrc, Ddst, M> {
    pub src_arena: Dsrc,
    pub dst_arena: Ddst,
    pub mappings: M,
}

impl<HAST, Dsrc, Ddst, M: MappingStore + Default> From<(HAST, (Dsrc, Ddst))>
    for Mapper<HAST, Dsrc, Ddst, M>
{
    fn from((hyperast, (src_arena, dst_arena)): (HAST, (Dsrc, Ddst))) -> Self {
        let mappings = M::default();
        Self {
            hyperast,
            mapping: Mapping {
                src_arena,
                dst_arena,
                mappings,
            },
        }
    }
}

impl<HAST, Dsrc, Ddst, M: MappingStore> Mapper<HAST, Dsrc, Ddst, M> {
    pub fn mappings(&self) -> &M {
        &self.mapping.mappings
    }
    pub fn map<Dsrc2, Ddst2, Fsrc: Fn(Dsrc) -> Dsrc2, Fdst: Fn(Ddst) -> Ddst2>(
        self,
        f_src: Fsrc,
        f_dst: Fdst,
    ) -> Mapper<HAST, Dsrc2, Ddst2, M> {
        Mapper {
            hyperast: self.hyperast,
            mapping: self.mapping.map(f_src, f_dst),
        }
    }
    pub fn map_src<Dsrc2, Fsrc: Fn(Dsrc) -> Dsrc2>(
        self,
        f_src: Fsrc,
    ) -> Mapper<HAST, Dsrc2, Ddst, M> {
        self.map(f_src, |x| x)
    }
    pub fn map_dst<Ddst2, Fdst: Fn(Ddst) -> Ddst2>(
        self,
        f_dst: Fdst,
    ) -> Mapper<HAST, Dsrc, Ddst2, M> {
        self.map(|x| x, f_dst)
    }
}

impl<Dsrc, Ddst, M: MappingStore> Mapping<Dsrc, Ddst, M> {
    pub fn map<Dsrc2, Ddst2, Fsrc: Fn(Dsrc) -> Dsrc2, Fdst: Fn(Ddst) -> Ddst2>(
        self,
        f_src: Fsrc,
        f_dst: Fdst,
    ) -> Mapping<Dsrc2, Ddst2, M> {
        Mapping {
            src_arena: f_src(self.src_arena),
            dst_arena: f_dst(self.dst_arena),
            mappings: self.mappings,
        }
    }
}

impl<HAST: HyperASTShared, Dsrc, Ddst, M> HyperASTShared for Mapper<HAST, Dsrc, Ddst, M> {
    type IdN = HAST::IdN;

    type Idx = HAST::Idx;

    type Label = HAST::Label;
}

impl<'a, HAST: HyperAST, Dsrc, Ddst, M> hyperast::types::NLending<'a, HAST::IdN>
    for Mapper<HAST, Dsrc, Ddst, M>
{
    type N = <HAST as hyperast::types::NLending<'a, HAST::IdN>>::N;
}

impl<'a, HAST: HyperAST, Dsrc, Ddst, M> hyperast::types::AstLending<'a>
    for Mapper<HAST, Dsrc, Ddst, M>
{
    type RT = <HAST as hyperast::types::AstLending<'a>>::RT;
}

impl<HAST: HyperAST, Dsrc, Ddst, M> HyperAST for Mapper<HAST, Dsrc, Ddst, M> {
    type NS = HAST::NS;

    fn node_store(&self) -> &Self::NS {
        self.hyperast.node_store()
    }

    type LS = HAST::LS;

    fn label_store(&self) -> &Self::LS {
        self.hyperast.label_store()
    }

    type TS = HAST::TS;
}
