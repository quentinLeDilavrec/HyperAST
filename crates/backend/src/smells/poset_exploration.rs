use hyperast::position::position_accessors::SolvedPosition;

use super::IdN;
use super::Idx;

pub fn semi_interactive_poset_build<P>(
    b: &mut hyperast_gen_ts_tsquery::code2query::Builder<'_, P>,
    meta_simp: &hyperast_tsquery::Query,
) where
    P: Eq + Clone + SolvedPosition<IdN> + Sync + Send,
{
    #[derive(PartialEq, Eq, Debug)]
    enum Phase {
        Uniq,
        SimpEq,
        Removes,
        RemovesAll,
    }

    let mut active_size = b.dedup.len() - 1;
    let mut active: Vec<_> = b.actives(active_size);
    let mut phase = Phase::Uniq;
    loop {
        log::info!("dedup len: {}", b.dedup.len());
        log::info!("active_size: {}", active_size);
        log::info!("actives: {}", active.len());
        // TODO find an alternative to phases
        // NOTE simp_eq creates larger pattern
        if phase == Phase::Uniq {
            let (rms, _already) = b.uniques_par_par(active_size, &mut active);
            b.dedup_uniques_par(active_size, rms);
        } else if phase == Phase::SimpEq {
            let (simps, already) = b.simp_eq(active_size, &mut active);
            let act = b.dedup_uniques_par2(active_size, simps);
            // active.extend(already);
            // removesall_par_par removes_par_par
            if let Some(cid) = meta_simp.capture_index_for_name("rm.all") {
                active = act;
                let rms = b.removesall_par_par(active_size, &mut active, cid);
                b.dedup_uniques_par2(active_size, rms);
                active = already;
                let rms = b.removesall_par_par(active_size, &mut active, cid);
                b.dedup_uniques_par2(active_size, rms);
            }
        } else if phase == Phase::RemovesAll {
            if let Some(cid) = meta_simp.capture_index_for_name("rm.all.full") {
                let rms = b.removesall_par_par(active_size, &mut active, cid);
                b.dedup_uniques_par2(active_size, rms);
            }
        } else if phase == Phase::Removes {
            for a in &active {
                log::info!("try remove:\n{}", b.lattice.pretty(&a));
            }
            let rms = b.removes_par_par(active_size, &mut active);
            for (_, (a, _)) in &rms {
                log::info!("to remove:\n{}", b.lattice.pretty(&a));
            }
            b.dedup_removes_par(active_size, &mut active, rms);
            log::info!("actives after removes: {}", active.len());
        }

        if b.between(&mut active_size, &mut active) {
            log::info!("finish phase: {phase:?}");
            if phase == Phase::Uniq {
                active_size = b.dedup.len() - 1;
                active = b.actives(active_size);
                phase = Phase::SimpEq;
            } else if phase == Phase::SimpEq {
                active_size = b.dedup.len() - 1;
                active = b.actives(active_size);
                phase = Phase::RemovesAll;
            } else if phase == Phase::RemovesAll {
                active_size = b.dedup.len() - 1;
                active = b.actives(active_size);
                phase = Phase::Removes;
            } else {
                // phase = Phase::Uniq;
                // active_size = b.dedup.len() - 1;
                break;
            }
        }
    }
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
