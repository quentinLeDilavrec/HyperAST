use hyperast::position::position_accessors::SolvedPosition;

use crate::smells::IdNQ;

use super::IdN;
use super::Idx;

pub fn semi_interactive_poset_build<P>(
    b: &mut hyperast_gen_ts_tsquery::code2query::Builder<'_, P>,
    meta_simp: &hyperast_tsquery::Query,
    mut timeout: impl FnMut() -> bool,
    size_threshold: impl Fn(usize) -> usize,
) where
    P: Eq + Clone + SolvedPosition<IdN> + Sync + Send,
{
    #[derive(PartialEq, Eq, Debug)]
    enum Phase {
        Uniq,
        Removes,
        RemovesAll,
        SimpEq,
        RemovesAll2,
        Removes2,
    }

    type BySize<T> = Vec<T>;
    type SimpEqResult = IdN;
    type SimpEqSource = IdN;
    type SimpEqedSet = Vec<(SimpEqResult, Vec<SimpEqSource>)>;
    // let mut simp_eq_valid_by_construction: BySize<SimpEqedSet> = vec![];

    let mut too_slow = hyperast_gen_ts_tsquery::code2query::DedupBySize2::<
        hyperast_gen_ts_tsquery::code2query::TR<P>,
    >::default();

    let mut active_size = b.dedup.len() - 1;
    let mut active: Vec<_> = b.actives(active_size);
    let mut phase = Phase::Uniq;
    loop {
        if timeout() {
            break;
        }
        log::info!("dedup len: {}", b.dedup.len());
        log::info!("active_size: {}", active_size);
        log::info!("actives: {}", active.len());
        // TODO find an alternative to phases
        // NOTE simp_eq creates larger pattern
        if phase == Phase::Uniq {
            // do it first to reduce noise and remove as many patts as possible
            let (rms, _already) = b.uniques_par_par(active_size, &mut active);
            // b.dedup_uniques_par(active_size, rms);
            // avoid hanging, by ignoring the patterns not shrinking enough
            use hyperast_gen_ts_tsquery::code2query::dedup_patterns_by_metric;
            let mut by_size = b.by_pattern_size(rms);
            let mut largest_by_size = by_size.split_off(&size_threshold(active_size));
            let act = dedup_patterns_by_metric(by_size, &mut b.dedup);
            active.extend(act);
            log::info!("actives after remove all: {}", active.len());

            hyperast_gen_ts_tsquery::code2query::filter_by_key_par(&mut largest_by_size, &b.dedup);
            let act = dedup_patterns_by_metric(largest_by_size, &mut too_slow);
            log::info!("too_slow added: {}", act.len());
        } else if phase == Phase::SimpEq {
            // the most tricky simp phase, as it adds a predicate and their captures
            // only do it on patterns under size threshold
            let (simps, not) = b.simp_eq(&mut active);
            let act = {
                b.dedup_uniques_par2(active_size, simps)

                // // avoid hanging, by ignoring the patterns not shrinking enough
                // use hyperast_gen_ts_tsquery::code2query::dedup_patterns_by_metric;
                // let mut by_size = b.by_pattern_size(simps);
                // {
                //     let max_active = by_size.last_key_value().map_or(active_size, |x| *x.0);
                //     let len = max_active.max(b.dedup.len());
                //     b.dedup.resize(len, Default::default());
                // }
                // let shrank_enough =
                //     by_size.split_off(&skink_threshold.min(active_size / skink_factor));
                // let act = dedup_patterns_by_metric(shrank_enough, &mut b.dedup);
                // log::info!("actives after remove all: {}", active.len());

                // hyperast_gen_ts_tsquery::code2query::filter_by_key_par(&mut by_size, &b.dedup);
                // let _act = dedup_patterns_by_metric(by_size, &mut too_slow);
                // log::info!("too_slow added: {}", _act.len());
                // act
            };
            if let Some(cid) = meta_simp.capture_index_for_name("rm.all") {
                active = act; // first rm on the ones that were simp
                let rms = b.removesall_par_par(active_size, &mut active, cid);
                let rms = b.repair_par(rms);
                b.dedup_uniques_par2(active_size, rms);
                active = not.clone(); // then the other that were not simp
                let rms = b.removesall_par_par(active_size, &mut active, cid);
                let rms = b.repair_par(rms);
                b.dedup_uniques_par2(active_size, rms);
            }
            // TODO link all found simpeq (then rm.all) to previous respective simpeq action,
            // ie. follow one back then re-apply same simpeq action and link to result
            // let call this a reduction and model it as TR::SimpEqReduction(TR)
        } else if phase == Phase::RemovesAll || phase == Phase::RemovesAll2 {
            // do not use naively after simp_eq as it might remove captures
            if let Some(cid) = meta_simp.capture_index_for_name("rm.all.full") {
                let rms = b.removesall_par_par(active_size, &mut active, cid);
                let rms = if phase == Phase::RemovesAll2 {
                    b.repair_par(rms)
                } else {
                    rms
                };
                // TODO add post-removal to make invalid patterns valid again

                // avoid hanging, by ignoring the patterns not shrinking enough
                use hyperast_gen_ts_tsquery::code2query::dedup_patterns_by_metric;
                let mut by_size = b.by_pattern_size(rms);
                let mut largest_by_size = by_size.split_off(&size_threshold(active_size));
                let act = dedup_patterns_by_metric(by_size, &mut b.dedup);
                active.extend(act);
                log::info!("actives after remove all: {}", active.len());

                hyperast_gen_ts_tsquery::code2query::filter_by_key_par(
                    &mut largest_by_size,
                    &b.dedup,
                );
                let act = dedup_patterns_by_metric(largest_by_size, &mut too_slow);
                log::info!("too_slow added: {}", act.len());
            }
        } else if phase == Phase::Removes || phase == Phase::Removes2 {
            // do not use naively after simp_eq as it might remove captures
            for a in &active {
                log::trace!("try remove: {:?}", b.lattice.pretty(&a));
            }
            let rms = b.removes_par_par(active_size, &mut active);
            for (a, _) in &rms {
                log::info!("to remove: {:?}", b.lattice.pretty(&a));
            }
            let rms = if phase == Phase::Removes2 {
                b.repair_par(rms)
            } else {
                rms
            };
            for (a, _) in &rms {
                log::info!("repaired: {:?}", b.lattice.pretty(&a));
            }
            // TODO add post-removal to make invalid patterns valid again
            // not that difficult, just need to remove predicate using absent capture
            // and then also remove the unused captures to help with deduplication
            // TODO or put invalid patterns in a separated set
            // detecting invalid is easy, for now I only see capture predicates missing captures
            // then can try to do removals
            // NOTE directly removing the rest is probably faster

            // reduce hanging, by ignoring the patterns not shrinking enough
            use hyperast_gen_ts_tsquery::code2query::dedup_patterns_by_metric;
            let mut by_size = b.by_pattern_size(rms);
            let mut largest_by_size = by_size.split_off(&size_threshold(active_size));
            let act = dedup_patterns_by_metric(by_size, &mut b.dedup);
            active.extend(act);
            log::info!("actives after removes: {}", active.len());

            hyperast_gen_ts_tsquery::code2query::filter_by_key_par(&mut largest_by_size, &b.dedup);
            let act = dedup_patterns_by_metric(largest_by_size, &mut too_slow);
            log::info!("too_slow added: {}", act.len());
        }

        if b.between(&mut active_size, &mut active) {
            log::info!("finish phase: {phase:?}");
            if phase == Phase::Uniq {
                active_size = b.dedup.len() - 1;
                active = b.actives(active_size);
                phase = Phase::RemovesAll;
            } else if phase == Phase::RemovesAll {
                active_size = b.dedup.len() - 1;
                active = b.actives(active_size);
                phase = Phase::Removes;
            } else if phase == Phase::Removes {
                active_size = b.dedup.len() - 1;
                // this phase produce many variants so only reasonably sized patterns are considered
                // tips consider more patterns in remove all
                // active_size = active_size.min(2000);
                phase = Phase::SimpEq;
                // this phase is complex, so only reasonably sized patterns are considered
                // tips consider more patterns in remove all
                active_size = active_size.min(size_threshold(0));
                active = b.actives(active_size);
            } else if phase == Phase::SimpEq {
                phase = Phase::RemovesAll2;
                active_size = b.dedup.len() - 1;
                active = b.actives(active_size);
                active_size = active_size.min(size_threshold(0));
                // simp_eq_valid_by_construction = find_simpeqs_ascending(b, active_size).collect();
                // dbg!(simp_eq_valid_by_construction.len());
                // dbg!(
                //     simp_eq_valid_by_construction
                //         .iter()
                //         .map(|x| x.len())
                //         .collect::<Vec<_>>()
                // );
            } else if phase == Phase::RemovesAll2 {
                phase = Phase::Removes2;
                active_size = b.dedup.len() - 1;
                active = b.actives(active_size);
                active_size = active_size.min(size_threshold(0));
            } else if phase == Phase::Removes2 {
                break;
            } else {
                unreachable!();
            }
        }
    }
    // let simp_eq_all = collect_simpeqs(b, b.dedup.len() - 1);
    // dbg!(simp_eq_all.len());
    // dbg!(simp_eq_all.iter().map(|x| x.len()).collect::<Vec<_>>());

    // remove invalid simp EQed patterns
    // * find all simpEQs
    // * find all constructively valid simp EQed patterns
    //   for each patt at source of simp EQ
    //   * find a path to init without simp EQ, they are valid by construction

    // done in `simp_eq_valid_by_construction`

    // * find all patterns leading to valid ones
    //   for each constructively valid simp EQed pattern
    //   * find reachable intermediate patterns, they should be valid
    //     explanation simp eq are made invalid by partially removing elements of query produced by simp eq
    //       if it reaches a valid simp EQed pattern it must also now be valid,
    //         by the way all simp EQ are done in one shot such that only one instance exists at a time
    //         it can be verified by checking for the absence of paths in the poet with more than one simpeq

    // lets compute a topological sort of the patterns
}

fn find_simpeqs_ascending<P>(
    b: &hyperast_gen_ts_tsquery::code2query::Builder<'_, P>,
    active_size: usize,
) -> impl Iterator<Item = Vec<(IdNQ, Vec<IdNQ>)>>
where
    P: Eq + Clone + SolvedPosition<IdN> + Sync + Send,
{
    use hyperast_gen_ts_tsquery::code2query::TR;
    let simp_eqs = |v: &Vec<TR<P>>| {
        v.iter()
            .filter_map(|x| match x {
                TR::SimpEQ(x) => Some(*x),
                _ => None,
            })
            .collect::<Vec<_>>()
    };
    (0..active_size).into_iter().map(move |size| {
        (b.dedup[size].iter())
            .filter_map(|x| {
                let v = simp_eqs(x.1);
                if v.is_empty() {
                    return None;
                }
                Some((*x.0, v))
            })
            .collect::<Vec<_>>()
    })
}

const Q: &str = r#"(named_node
    (identifier) (#EQ? "try_statement")
) @uniq
(named_node
    (identifier) (#EQ? "catch_type")
) @rm.all.full
(named_node
    (identifier) (#EQ? "string_literal")
) @rm.all.full
(named_node
    (identifier) (#EQ? "decimal_integer_literal")
) @rm.all.full
(named_node
    (identifier) (#EQ? "null_literal")
) @rm.all.full
(named_node
    (identifier) (#EQ? "generic_type")
) @rm.all.full
(named_node
    (identifier) (#EQ? "cast_expression")
    .
) @rm.all.full
(named_node
    (identifier) (#EQ? "unary_expression")
    (named_node
        (identifier) .
    )
) @rm
(named_node
    (identifier) (#EQ? "object_creation_expression")
    (named_node
        (identifier) (#EQ? "argument_list")
        .
    )
) @rm.all.full
(named_node
    (identifier) (#EQ? "method_invocation")
    (named_node
        (identifier) (#EQ? "identifier")
    ) .
    (predicate
        (identifier) (#EQ? "EQ")
        (parameters
            (string)
        )
    ) .
) @rm
(named_node
    (identifier) (#EQ? "method_invocation") .
    (named_node
        (identifier) (#EQ? "identifier") .
    ) .
    (predicate
        (identifier) (#EQ? "EQ")
        (parameters
            (string)
        )
    ) .
    (named_node
        (identifier) (#EQ? "argument_list")
        .
    ) .
) @rm
(named_node
    (identifier) (#EQ? "method_invocation")
    (named_node
        (identifier) (#EQ? "identifier") .
    ) .
    (predicate
        (identifier) (#EQ? "EQ")
        (parameters
            (string)
        )
    ) .
) @rm
(named_node
    (identifier) (#EQ? "method_invocation")
    (named_node
        (identifier) (#EQ? "identifier")
    ) .
    (predicate
        (identifier) (#EQ? "EQ")
        (parameters
            (string)
        )
    ) .
    (named_node
        (identifier) (#EQ? "argument_list")
        (named_node
            (identifier) (#EQ? "identifier")
            .
        ) .
    )
) @rm
(named_node
    (identifier) (#EQ? "method_invocation")
    (named_node
        (identifier) (#EQ? "identifier")
    ) .
    (predicate
        (identifier) (#EQ? "EQ")
        (parameters
            (string)
        )
    ) .
    (named_node
        (identifier) (#EQ? "argument_list")
        .
    )
) @rm
(named_node
    (identifier) (#EQ? "block") .
    (named_node
        (identifier) (#EQ? "try_statement")
    ) @focus .
    (capture
        (identifier) (#EQ? "_root")
    )
)
(named_node
    (identifier) (#EQ? "method_invocation") .
    (named_node
        (identifier) (#EQ? "identifier")
    ) @rm.all.full .
    (predicate
        (identifier) (#EQ? "EQ")
    ) @rm.all.full .
    (named_node
        (identifier) (#EQ? "identifier")
    ) .
    (predicate
        (identifier) (#EQ? "EQ")
    )
)
(named_node
    (identifier) (#EQ? "catch_formal_parameter")
    (named_node
        (identifier) (#EQ? "identifier")
    ) .
    (predicate
        (identifier) (#EQ? "EQ")
        (parameters
            (string) @label
        )
    ) @pred
) @rm.all
(named_node
    (identifier) (#EQ? "catch_clause")
    (named_node
        (identifier) (#EQ? "catch_formal_parameter")
        (named_node
            (identifier) (#EQ? "identifier")
        ) .
        (predicate
            (identifier) (#EQ? "EQ")
            (parameters
                (string)
            )
        )
    ) @rm.all.full
    (named_node
        (identifier) (#EQ? "block")
        .
    )
)
(named_node
    (identifier) (#EQ? "catch_clause")
    .
    (named_node
        (identifier) (#EQ? "block")
    ) @rm.all.full
)
(named_node
    (identifier) (#EQ? "argument_list")
    (named_node
        (identifier) (#EQ? "method_invocation") .
        (named_node
            (identifier) (#EQ? "identifier")
        ) .
        (predicate
            (identifier) (#EQ? "EQ")
            (parameters
                (string) @label
            )
        ) @pred
    )
) @rm.all
(named_node
    (identifier) (#EQ? "argument_list")
    (named_node
        (identifier) (#EQ? "method_invocation") .
        (named_node
            (identifier) (#EQ? "identifier")
        ) .
        (predicate
            (identifier) (#EQ? "EQ")
            (parameters
                (string)
            )
        )
    ) @rm
)
(named_node
    (identifier) (#EQ? "argument_list")
    (named_node
        (identifier) (#EQ? "identifier") .
    ) .
    (predicate
        (identifier) (#EQ? "EQ")
        (parameters
            (string) @label
        )
    ) @pred
) @rm.all
(named_node
    (identifier) (#EQ? "method_invocation")
    (named_node
        (identifier) (#EQ? "identifier") .
    )
    (predicate
        (identifier) (#EQ? "EQ")
        (parameters
            (string)
        )
    )
    (named_node
        (identifier) (#EQ? "argument_list")
        .
    )
) @rm.all.full
(named_node .
    (identifier) .
    "/" @rm.all.full .
    (identifier) @rm.all.full
)
"#;
const Q2: &str = r#"
(named_node
    (identifier) (#EQ? "method_invocation")
    (named_node
        (identifier) (#EQ? "identifier")
    ) .
    (predicate
        (identifier) (#EQ? "EQ")
        (parameters
            (string)
        )
    )
) @rm
"#;

#[test]
fn test_match_for_removes_not_matching() {
    let meta_simp = hyperast_tsquery::Query::new(Q2, hyperast_gen_ts_tsquery::language())
        .map_err(|e| format!("error in meta_simp: {e}"))
        .unwrap();
    let text = r#"(try_statement
      (block
        (expression_statement
          (method_invocation
            (identifier
            ) (#EQ? "addAll")
          )
        )
        (expression_statement
          (method_invocation
            (identifier) (#EQ? "fail")
            (argument_list)
          )
        )
      )
      (catch_clause)
    ) @_root"#
        .as_bytes();
    use hyperast_gen_ts_tsquery as t_gen;
    let mut query_store = hyperast::store::SimpleStores::<t_gen::types::TStore>::default();
    let mut md_cache = Default::default();
    let mut tree_gen = t_gen::no_fmt_legion::TsQueryTreeGen::new(&mut query_store, &mut md_cache);

    let tree = match t_gen::no_fmt_legion::tree_sitter_parse(text) {
        Ok(t) => t,
        Err(t) => t,
    };
    println!("{}", tree.root_node().to_sexp());
    let cid = meta_simp.capture_index_for_name("rm").unwrap();
    let full_node = tree_gen.generate_file(b"", text, tree.walk());
    let query = full_node.local.compressed_node;
    let mut pos = hyperast::position::structural_pos::CursorWithPersistence::new(query);
    let cursor = hyperast_tsquery::hyperast_opt::TreeCursor::new(&query_store, pos);
    let mut matches = meta_simp.matches(cursor);
    loop {
        let Some(m) = matches.next() else {
            break;
        };
        dbg!(m.pattern_index);
        log::info!("found match {}", m.pattern_index.to_usize());
        for p in m.nodes_for_capture_index(cid) {
            dbg!();
            return;
        }
    }
    panic!()
}
