use const_chunks::IteratorConstChunks;
use hyper_diff::decompressed_tree_store::CompletePostOrder;
use hyper_diff::decompressed_tree_store::lazy_post_order::LazyPostOrder;
use hyper_diff::mappings::VecStore;
use hyper_diff::matchers::{Decompressible, Mapper};
use hyperast::store::nodes::compo;
use hyperast::types::LendT;
use hyperast::types::WithMetaData;
use hyperast::types::{HyperAST, HyperASTShared};
use hyperast::types::{WithHashs, WithStats};

use super::Prep;

#[allow(type_alias_bounds)]
type CDS<HAST: HyperASTShared> = Decompressible<HAST, CompletePostOrder<HAST::IdN, u32>>;
type M = hyper_diff::mappings::VecStore<u32>;

#[derive(Clone)]
pub struct Routine<F> {
    pub name: String,
    pub prep: Prep,
    pub routine: F,
}

type IdD = u32;
pub struct Routines<A>(std::marker::PhantomData<A>);

pub type OwnedLazyMapping<IdN, const S: usize = 2> =
    ([LazyPostOrder<IdN, IdD>; S], [VecStore<IdD>; S]);
pub trait WithSetup {
    fn run<I, O, S: FnMut() -> I, R: FnMut(I) -> O>(&mut self, s: S, r: R);
}

#[repr(transparent)]
struct Vek<T>(Vec<T>);
impl<F> std::ops::AddAssign<Routine<F>> for Vek<Routine<F>> {
    fn add_assign(&mut self, other: Routine<F>) {
        self.0.push(other);
    }
}
type RVec<R> = Vek<Routine<R>>;

impl<HAST, IdN, B: WithSetup> Routines<(HAST, IdN, B)> {
    pub fn lazy_to_complete<const MAX_SIZE: usize, const S: usize>()
    -> impl Iterator<Item = Routine<fn(&mut B, &HAST, OwnedLazyMapping<IdN, S>)>>
    where
        IdN: hyperast::types::UniformNodeId,
        for<'a> &'a HAST: HyperASTShared<IdN = IdN>,
        for<'a> &'a HAST: HyperAST + Copy,
        for<'t, 'a> LendT<'t, &'a HAST>: WithStats,
        for<'t, 'a> LendT<'t, &'a HAST>: WithHashs,
        for<'a> <&'a HAST as HyperASTShared>::Label: std::cmp::Eq,
        for<'t, 'a> LendT<'t, &'a HAST>: WithMetaData<compo::StmtCount>,
        for<'t, 'a> LendT<'t, &'a HAST>: WithMetaData<compo::MemberImportCount>,
    {
        use hyper_diff::matchers::heuristic::cd;
        use hyper_diff::matchers::heuristic::gt;
        let mut routines: RVec<fn(&mut B, &HAST, OwnedLazyMapping<IdN, S>)> = Vek(vec![]);
        macro_rules! routine {
            (@in $prep:ident; $f:expr; $mpr:pat => $matcher:expr) => {
                routines += Routine { routine: |b, hyperast, (owned, mappings)| { b.run(
                    || (owned.clone(), mappings.clone()),
                    |(owned, mut mappings)| {
                        let mut out = std::array::from_fn::<_, S, _>(|_| {
                            None
                        });
                        let mut it = owned.into_iter();
                        let tmp = CDS::from(Decompressible {
                            hyperast,
                            decomp: it.next().unwrap(),
                        })
                        .decomp;
                        out[0] = Some(tmp);
                        it.zip(mappings.iter_mut())
                            .enumerate()
                            .for_each(|(i, (b, c))| {
                                let a = out[i].take().unwrap();
                                let b = CDS::from(Decompressible {
                                    hyperast,
                                    decomp: b,
                                });
                                let mapper = Mapper {
                                    hyperast,
                                    mapping: hyper_diff::matchers::Mapping {
                                        src_arena: Decompressible {
                                            hyperast,
                                            decomp: a,
                                        },
                                        dst_arena: b,
                                        mappings: c,
                                    },
                                };
                                let mapper = match mapper { $mpr => {$matcher}};
                                let a = mapper.mapping.src_arena.decomp;
                                out[i] = Some(a);

                                let b = mapper.mapping.dst_arena.decomp;
                                out[i + 1] = Some(b);
                            });
                        (out, mappings)
                        // let mapper = Mapper::prep(hyperast, mappings, owned);
                        // let mapper = mapper.map(CDS::from, CDS::from);
                        // let mapper = match mapper { $mpr => {$matcher}};
                        // mapper.mapping.map(|a| a.decomp, |a| a.decomp)
                    },
                );}, name: $f, prep: Prep::$prep };
            };
            ($f:expr; [$($S:ident),*]  $($mpr:tt)+) => {
                if true && $($S > 0)&&* {
                    routine!(@in GT; format!($f, $($S),*); $($mpr)*);
                }
            };
            ($f:expr; 0==[$($S:ident),*]  $($mpr:tt)+) => {
                if true && $($S == 0)&&* {
                    routine!(@in GT; format!($f); $($mpr)*);
                }
            };
            (CD; $f:expr; [$($S:ident),*]  $($mpr:tt)+) => {
                if true && $($S > 0)&&* {
                    routine!(@in CD; format!($f, $($S),*); $($mpr)*);
                }
            };
            (CD; $f:expr; 0==[$($S:ident),*]  $($mpr:tt)+) => {
                if true && $($S == 0)&&* {
                    routine!(@in CD; format!($f); $($mpr)*);
                }
            };
        }
        // routines += Routine {
        //     routine: |b, hyperast, (owned, mappings)| {
        //         b.run(
        //             || (owned.clone(), mappings.clone()),
        //             |(owned, mut mappings)| {
        //                 let mut out = std::array::from_fn::<_, S, _>(|_| {
        //                     MaybeUninit::<CompletePostOrder<_, _>>::uninit()
        //                 });
        //                 let mut it = owned.into_iter();
        //                 let mut tmp = CDS::from(Decompressible {
        //                     hyperast,
        //                     decomp: it.next().unwrap(),
        //                 })
        //                 .decomp;

        //                 std::mem::swap(unsafe { out[0].assume_init_mut() }, &mut tmp);
        //                 it.zip(mappings.iter_mut())
        //                     .enumerate()
        //                     .for_each(|(i, (b, c))| {
        //                         let a = std::mem::replace(&mut out[i], MaybeUninit::uninit());
        //                         let b = CDS::from(Decompressible {
        //                             hyperast,
        //                             decomp: b,
        //                         });
        //                         let mapper = Mapper {
        //                             hyperast,
        //                             mapping: hyper_diff::matchers::Mapping {
        //                                 src_arena: Decompressible {
        //                                     hyperast,
        //                                     decomp: unsafe { a.assume_init() },
        //                                 },
        //                                 dst_arena: b,
        //                                 mappings: c,
        //                             },
        //                         };
        //                         let mapper = gt::greedy_bottom_up_matcher::GreedyBottomUpMatcher::<
        //                             _,
        //                             M,
        //                             MAX_SIZE,
        //                         >::match_it(mapper);
        //                         let mut a = MaybeUninit::new(mapper.mapping.src_arena.decomp);
        //                         std::mem::swap(&mut out[i], &mut a);

        //                         let mut b = MaybeUninit::new(mapper.mapping.dst_arena.decomp);
        //                         std::mem::swap(&mut out[i + 1], &mut b);
        //                     });
        //                 (out, mappings)
        //             },
        //         );
        //     },
        //     name: "".to_string(),
        //     prep: Prep::CD,
        // };
        routine!("Greedy_{}"; [MAX_SIZE]
            mapper => gt::greedy_bottom_up_matcher::
                GreedyBottomUpMatcher::<_, M, MAX_SIZE>::match_it(mapper));
        routine!("Hybrid_{}"; [MAX_SIZE]
            mapper => gt::hybrid_bottom_up_matcher::
                HybridBottomUpMatcher::<_, M, MAX_SIZE>::match_it(mapper));
        routine!("Stable_{}"; [MAX_SIZE]
            mapper => gt::marriage_bottom_up_matcher::
                MarriageBottomUpMatcher::<_, M, MAX_SIZE>::match_it(mapper));
        routine!("StableHybrid_{}"; [MAX_SIZE]
            mut mapper => {
                mapper.bottom_up_stable_with_similarity_threshold_and_recovery(
                    Mapper::adaptive_threshold,
                    hyper_diff::similarity_metrics::SimilarityMeasure::chawathe,
                    Mapper::last_chance_match_hybrid::<M, MAX_SIZE>,
                );
                mapper
        });
        routine!("gt_CD_{}"; [MAX_SIZE]
            mapper => cd::bottom_up_matcher::
                BottomUpMatcher::<_, MAX_SIZE>::match_it(mapper));
        routine!(CD; "CD_{}"; [MAX_SIZE]
            mapper => cd::bottom_up_matcher::
                BottomUpMatcher::<_, MAX_SIZE>::match_it(mapper));

        routine!("StableSimple"; 0==[MAX_SIZE]
            mut mapper => {
                mapper.bottom_up_stable_with_similarity_threshold_and_recovery(
                    |_, _, _| 1f64 / 2f64,
                    hyper_diff::similarity_metrics::SimilarityMeasure::chawathe,
                    Mapper::last_chance_match_histogram,
                );
                mapper
        });
        routine!("Simple"; 0==[MAX_SIZE]
            mapper => gt::simple_bottom_up_matcher
                ::SimpleBottomUpMatcher::<_>::match_it(mapper));
        routine!("Xy"; 0==[MAX_SIZE]
            mapper => hyper_diff::matchers::heuristic::xy_bottom_up_matcher
                ::XYBottomUpMatcher::<_>::match_it(mapper));
        routines.0.into_iter()
    }
}

impl<HAST, IdN, B: WithSetup> Routines<(HAST, IdN, B)> {
    pub fn lazy_to_lazy<const MAX_SIZE: usize, const S: usize>()
    -> impl Iterator<Item = Routine<fn(&mut B, &HAST, OwnedLazyMapping<IdN, S>)>>
    where
        IdN: hyperast::types::UniformNodeId,
        for<'a> &'a HAST: HyperASTShared<IdN = IdN>,
        for<'a> &'a HAST: HyperAST + Copy,
        for<'t, 'a> LendT<'t, &'a HAST>: WithStats,
        for<'t, 'a> LendT<'t, &'a HAST>: WithHashs,
        for<'a> <&'a HAST as HyperASTShared>::Label: std::cmp::Eq,
        for<'t, 'a> LendT<'t, &'a HAST>: WithMetaData<compo::StmtCount>,
        for<'t, 'a> LendT<'t, &'a HAST>: WithMetaData<compo::MemberImportCount>,
    {
        use hyper_diff::matchers::heuristic::cd;
        use hyper_diff::matchers::heuristic::gt;
        let mut routines: RVec<fn(&mut B, &HAST, OwnedLazyMapping<IdN, S>)> = Vek(vec![]);
        macro_rules! routine {
            (@in $prep:ident; $f:expr; $mpr:pat => $matcher:expr) => {
                routines += Routine { routine: |b, hyperast, (owned, mappings)| { b.run(
                    || (owned.clone(), mappings.clone()),
                    |(mut owned, mut mappings)| {
                        owned
                            .iter_mut()
                            .const_chunks::<2>()
                            .zip(mappings.iter_mut().const_chunks::<2>())
                            .for_each(|([a, b], [c, _])| {
                                let mpr = Mapper {
                                    hyperast,
                                    mapping: hyper_diff::matchers::Mapping {
                                        src_arena: Decompressible {
                                            hyperast,
                                            decomp: a,
                                        },
                                        dst_arena: Decompressible {
                                            hyperast,
                                            decomp: b,
                                        },
                                        mappings: c,
                                    },
                                };
                            let _mpr = match mpr { $mpr => {$matcher}};
                        });
                        (owned, mappings)
                        // let mut mapper = Mapper::prep(hyperast, mappings, owned);
                        // let mpr = mapper.mut_decompressible();
                        // let _mpr = match mpr { $mpr => {$matcher}};
                        // mapper.mapping.map(|a| a.decomp, |a| a.decomp)
                    },
                );}, name: $f, prep: Prep::$prep};
            };
            (CD; $f:expr; [$($S:ident),*] $($mpr:tt)+) => {
                if true && $($S > 0)&&* {
                    routine!(@in CD; format!($f, $($S),*); $($mpr)*);
                }
            };
            (CD; $f:expr; 0==[$($S:ident),*] $($mpr:tt)+) => {
                if true && $($S == 0)&&* {
                    routine!(@in CD; format!($f); $($mpr)*);
                }
            };
            ($f:expr; [$($S:ident),*] $($mpr:tt)+) => {
                if true && $($S > 0)&&* {
                    routine!(@in GT; format!($f, $($S),*); $($mpr)*);
                }
            };
            ($f:expr; 0==[$($S:ident),*] $($mpr:tt)+) => {
                if true && $($S == 0)&&* {
                    routine!(@in GT; format!($f); $($mpr)*);
                }
            };
        }
        routines +=
            Routine {
                routine: |b, hyperast, (owned, mappings)| {
                    b.run(
                        || (owned.clone(), mappings.clone()),
                        |(mut owned, mut mappings)| {
                            owned
                                .iter_mut()
                                .const_chunks::<2>()
                                .zip(mappings.iter_mut().const_chunks::<2>())
                                .for_each(|([a, b], [c, _])| {
                                    let mpr = Mapper {
                                        hyperast,
                                        mapping: hyper_diff::matchers::Mapping {
                                            src_arena: Decompressible {
                                                hyperast,
                                                decomp: a,
                                            },
                                            dst_arena: Decompressible {
                                                hyperast,
                                                decomp: b,
                                            },
                                            mappings: c,
                                        },
                                    };
                                    let _mpr = gt::lazy_greedy_bottom_up_matcher::
                        LazyGreedyBottomUpMatcher::<_, M, MAX_SIZE>::match_it(mpr);
                                });
                            (owned, mappings)
                            // let mut mapper = Mapper::prep(hyperast, mappings, owned);
                            // let mpr = mapper.mut_decompressible();
                            // let _mpr = match mpr { $mpr => {$matcher}};
                            // mapper.mapping.map(|a| a.decomp, |a| a.decomp)
                        },
                    );
                },
                name: "aaa".to_owned(),
                prep: Prep::CD,
            };
        routine!("LazyGreedy_{}"; [MAX_SIZE]
            mpr => gt::lazy_greedy_bottom_up_matcher::
                LazyGreedyBottomUpMatcher::<_, M, MAX_SIZE>::match_it(mpr));
        routine!("LazyHybrid_{}"; [MAX_SIZE]
            mpr => gt::lazy_hybrid_bottom_up_matcher::
                LazyHybridBottomUpMatcher::<_, M, MAX_SIZE>::match_it(mpr));
        routine!("LazyStable_{}"; [MAX_SIZE]
            mpr => gt::lazy_marriage_bottom_up_matcher::
                LazyMarriageBottomUpMatcher::<_, M, MAX_SIZE>::match_it(mpr));
        routine!("LazyStableHybrid_{}"; [MAX_SIZE]
            mpr => gt::lazy_hybrid_marriage_bottom_up_matcher::
                LazyHybridMarriageBottomUpMatcher::<_, M, MAX_SIZE>::match_it(mpr));
        routine!("gt_LazyCD_{}"; [MAX_SIZE]
            mpr => cd::lazy_bottom_up_matcher::
                BottomUpMatcher::<_, MAX_SIZE>::match_it(mpr));
        routine!(CD; "LazyCD_{}"; [MAX_SIZE]
            mpr => cd::lazy_bottom_up_matcher::
                BottomUpMatcher::<_, MAX_SIZE>::match_it(mpr));

        routine!("LazyStableSimple"; 0==[MAX_SIZE]
            mpr => gt::lazy_simple_marriage_bottom_up_matcher
                ::LazySimpleMarriageBottomUpMatcher::<_>::match_it(mpr));
        routine!("LazySimple"; 0==[MAX_SIZE]
            mpr => gt::lazy_simple_bottom_up_matcher
                ::LazySimpleBottomUpMatcher::<_>::match_it(mpr));
        routine!("LazyXy"; 0==[MAX_SIZE]
            mpr => hyper_diff::matchers::heuristic::lazy_xy_bottom_up_matcher
                ::LazyXYBottomUpMatcher::<_>::match_it(mpr));
        routines.0.into_iter()
    }
}
