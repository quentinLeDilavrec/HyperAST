use axum::Json;
use hashbrown::HashSet;
use hyperast::nodes::TextSerializer;
use serde::{Deserialize, Serialize};
use std::ops::Range;
use tokio::time::Instant;

use hyper_diff::actions::Actions;
use hyperast::position::TreePathMut;
use hyperast::position::position_accessors::SolvedPosition;
use hyperast::position::position_accessors::{RootedPosition, WithPreOrderOffsets};
use hyperast::types::{Children, LabelStore, WithStats};
use hyperast_gen_ts_tsquery::code2query::QueryLattice;

use crate::SharedState;
use crate::utils::{IdN, LocalPieceOfCode, PieceOfCode};

pub(crate) mod matching;

mod diffing;

type Idx = u16;

#[derive(Deserialize, Clone)]
pub struct Param {
    user: String,
    name: String,
    commit: String,
    len: usize,
}

impl Param {
    pub fn repo(&self) -> hyperast_vcs_git::git::Repo {
        hyperast_vcs_git::git::Forge::Github.repo(&self.user, &self.name)
    }
}

#[derive(Deserialize, Clone)]
pub struct Diffs {
    user: String,
    name: String,
    commit: String,
    len: usize,
}

impl Diffs {
    pub fn repo(&self) -> hyperast_vcs_git::git::Repo {
        hyperast_vcs_git::git::Forge::Github.repo(&self.user, &self.name)
    }
}

#[derive(Deserialize, Clone)]
pub struct Examples {
    #[serde(default)]
    simple_matching: bool,
    #[serde(default)]
    prepro_matching: bool,
    /// the query configuring the query generation from examples,
    /// such as,
    /// ```scheme
    /// (identifier) @label ["{" ";" "." "try" "(" ")" "}" "catch" "import"] @skip (block ["{" "}"] @show) (block) @imm
    /// ```
    /// or
    /// ```scheme
    /// (identifier) (type_identifier)` same as `(identifier) @label (type_identifier) @label
    /// ```
    meta_gen: String,
    /// the query configuring the query simplification/generalization,
    /// such as,
    /// ```scheme
    /// (predicate (identifier) (#EQ? "EQ") (parameters (string) @label )) @pred
    /// ```
    meta_simp: String,
    /// the list of examples driving the query generation
    examples: Vec<ExamplesValue>,
}

#[derive(Debug, Serialize, Clone)]
pub enum SmellsError {
    Error(String),
}

#[derive(Serialize)]
pub struct SearchResults<G = self::lattice::G> {
    pub prepare_time: f64,
    pub search_time: f64,
    bad: Vec<SearchResult>,
    good: Vec<SearchResult>,
    additional: Vec<ExamplesValue>,
    graphs: G,
}

#[derive(serde::Deserialize, serde::Serialize, Debug, Clone)]
pub struct SearchResult<Q = String> {
    pub query: Q,
    /// the corresponding examples
    pub examples: Vec<usize>,
    // stats
    pub matches: usize,
    pub additional: Vec<usize>,
}

#[derive(Serialize)]
pub struct ExamplesResults {
    pub prepare_time: f64,
    pub search_time: f64,
    examples: Vec<ExamplesValue>,
    moves: Vec<(PieceOfCode, PieceOfCode)>,
}

#[derive(Deserialize, Clone, Serialize)]
pub struct ExamplesValue<Idx = usize> {
    before: PieceOfCode,
    after: PieceOfCode,
    deletes: Vec<Range<Idx>>,
    inserts: Vec<Range<Idx>>,
    moves: Vec<(Range<Idx>, Range<Idx>)>,
}

/// Compute smells from examples (extract from a pair of commits).
///
/// For simplicity, here, let's assume that provided changes are fixing the smells.
/// Changes can be inverted to simulate smell fixes if it is not the case.
/// meta_gen and meta_simp can also be used to change the behavior of the smell synthesis.
pub(crate) fn smells(
    examples: Examples,
    state: SharedState,
    path: Param,
) -> Result<SearchResults, String> {
    let now = Instant::now();
    let repo_spec = path.repo();
    let Param { commit, len, .. } = path;
    log::warn!("use len value={len}");
    let Examples {
        meta_gen,
        meta_simp,
        examples,
        simple_matching,
        prepro_matching,
    } = examples;
    let prepro_matching = if simple_matching {
        prepro_matching
    } else if prepro_matching {
        prepro_matching
    } else {
        true
    };

    let repo_handle = (state.repositories.read().unwrap())
        .get_config(repo_spec)
        .ok_or_else(|| "missing config for repository".to_string())?;
    let mut repository = repo_handle.fetch();
    log::warn!("done cloning {}", repository.spec);
    let commits = (state.repositories.write().unwrap())
        .pre_process_with_limit(&mut repository, "", &commit, 4)
        .map_err(|e| e.to_string())?;
    log::info!(
        "done construction of {commits:?} commits in {}",
        repository.spec.user()
    );
    let prepare_time = now.elapsed().as_secs_f64();
    let now = Instant::now();
    let src_oid = commits[0];
    let dst_oid = commits[1];
    use hyperast_vcs_git::processing::ConfiguredRepoTrait;
    let repo_handle = &repository;
    let repositories = state.repositories.read().expect("not poisoned");
    let commit_src = repositories
        .get_commit(repo_handle.config(), &src_oid)
        .ok_or("missing source commit")?;
    let _src_tr = commit_src.ast_root;
    let commit_dst = repositories
        .get_commit(repo_handle.config(), &dst_oid)
        .ok_or("missing destination commit")?;
    let dst_tr = commit_dst.ast_root;
    let with_spaces_stores = &repositories.processor.main_stores;

    // NOTE temporary solution, will be fixed when adding more polyglote facilities
    use hyperast_gen_ts_java as ts_gen;
    let sss: &hyperast::store::SimpleStores<ts_gen::types::TStore> = with_spaces_stores.with_ts();

    let meta_gen = hyperast_tsquery::Query::new(&meta_gen, ts_gen::language())
        .map_err(|e| format!("error in meta_gen: {e}"))?;

    let meta_simp = hyperast_tsquery::Query::new(&meta_simp, hyperast_gen_ts_tsquery::language())
        .map_err(|e| format!("error in meta_simp: {e}"))?;

    let ex_map: ExMap = examples
        .iter()
        .enumerate()
        .map(|(i, e)| {
            assert_eq!(e.before.commit, dst_oid);
            assert!(!e.before.path.is_empty());
            let (_, from) = hyperast::position::compute_position(
                dst_tr,
                &mut e.before.path.iter().map(|x| *x as u16),
                with_spaces_stores,
            );
            (from, i as u16)
        })
        .fold(Default::default(), |mut acc, x| {
            acc.entry(x.0).or_default().push(x.1);
            acc
        });
    let query_lattice = if false {
        QueryLattice::with_examples_by_size_try::<_, ts_gen::types::TIdN<_>>(
            sss,
            ex_map.keys(),
            &meta_gen,
            &meta_simp,
        )
    } else {
        let stores = sss;
        let b = QueryLattice::builder::<ts_gen::types::TStore, ts_gen::types::TIdN<_>, _>(
            stores,
            ex_map.keys(),
            &meta_gen,
            &meta_simp,
            &|x| (x.local.metrics.size, x.local.metrics.hashs.label),
        );
        let mut b = b.dedup_leaf_queries(|from: Vec<(_, (_, (u32, u32)))>| {
            hyperast_gen_ts_tsquery::code2query::group_by_size(from)
        });

        poset_exploration::semi_interactive_poset_build(&mut b, &meta_simp);
        log::trace!(
            "final lattice size: {}",
            b.dedup.iter().map(|x| x.len()).sum::<usize>()
        );
        b.post();
        b.build()
    };

    let bad: Vec<_> = query_lattice
        .iter_pretty()
        .filter(|x| 5 < x.1.len() && x.1.len() * 2 < ex_map.len())
        .map(|(s, x)| (s, std::borrow::Cow::Borrowed(x)))
        .take(10000)
        .collect();

    let mut graphs = {
        let g = lattice::Prep::extract_and_group(&query_lattice);
        g.describe();
        let f = |x: &IdN| TextSerializer::new(sss, *x).to_string();
        g.log(&f);
        g
    };

    // let mut idq_storage = vec![]; // temporary stuff for compatibility
    let bad = if bad.is_empty() {
        graphs
            .tops()
            .filter(|(q, _)| {
                let lang = hyperast_gen_ts_java::language();
                !q.is_empty() && hyperast_tsquery::Query::new(&q, lang).is_ok()
            })
            .map(|(s, x)| (s, std::borrow::Cow::Owned(x)))
            .collect()
    } else {
        bad
    };
    dbg!(bad.len());
    let matches = if simple_matching {
        matching::matches_default(with_spaces_stores, dst_tr, bad.iter().map(|x| x.0.as_str()))?
    } else if prepro_matching {
        let precomputeds = (state.repositories.read().unwrap())
            .get_precomp_query(*repo_handle.config(), "Java")
            .expect("some precomputed patterns should been provided");
        matching::matches_with_precomputeds(
            with_spaces_stores,
            dst_tr,
            bad.iter().map(|x| x.0.as_str()),
            precomputeds,
        )?
    } else {
        unreachable!()
        // TODO
        // let qqq = hyperast_tsquery::Query::big(
        //     &col.iter().map(|x| bad[x[0]].query.as_str()).collect::<Vec<_>>(),
        //     hyperast_gen_ts_java::language(),
        // )
        // .map_err(|e| e.to_string())?;
    };
    eprintln!("matches: {:?}", matches);
    eprintln!("bads: {:?}", bad);
    assert_eq!(bad.len(), matches.len());
    let mut bad: Vec<_> = (matches.iter().enumerate())
        .map(|(i, v)| SearchResult {
            query: bad[i].0.clone(),
            examples: examples4idqs(&ex_map, &query_lattice, &bad[i].1),
            matches: *v,
            additional: vec![],
        })
        .collect();
    dbg!(bad.len());

    bad.sort_by(|a, b| {
        let cmp = b.examples.len().cmp(&a.examples.len());
        cmp.then_with(|| b.query.len().cmp(&a.query.len()))
    });

    let good = vec![]; // TODO generate the smell fixes
    let additional = vec![]; // TODO generate the added code not actively involved with a smell fix

    let graphs = {
        let f = |x: &IdN| TextSerializer::new(sss, *x).to_string();
        graphs.compress(&f)
    };

    let search_time = now.elapsed().as_secs_f64();
    Ok(SearchResults {
        prepare_time,
        search_time,
        bad,
        good,
        additional,
        graphs: graphs,
    })
}

// offset of initial query in QueryLattice
type IdQ = u32;
// id of query pointing at a subtree in a hyperast
type IdNQ = IdN;
/// offset in input vec of provided examples
type ExOffset = u16;
/// map each unique Hyperast subtree (from examples) back to provided examples
type ExMap = std::collections::HashMap<IdN, Vec<ExOffset>>;
/// offset in list of bad smells patterns
type BOffset = usize;

/// reconstruct the original offsets to examples for given inits
/// (associated to a pattern in QueryLattice)
fn examples4idqs(
    ex_map: &std::collections::HashMap<IdN, Vec<ExOffset>>,
    query_lattice: &QueryLattice<&IdN>,
    inits: &[IdQ],
) -> Vec<usize> {
    let init_queries = inits.iter();
    let leaves = init_queries.map(|x: &IdQ| query_lattice.leaf(*x));
    let trs = leaves.filter_map(|x: IdNQ| query_lattice.raw_rels.get(&x));
    let flat_map = trs.flat_map(|x| {
        x.iter()
            .filter_map(|x| x.as_init())
            .flat_map(|x| ex_map.get(x))
    });
    let hset = flat_map.flatten().copied().collect::<HashSet<_>>();
    hset.into_iter().map(|x| x as usize).collect()
}

pub(crate) fn smells_ex_from_diffs(
    state: SharedState,
    path: Param,
) -> Result<ExamplesResults, String> {
    let now = Instant::now();
    let repo_spec = path.repo();
    let Param { commit, len, .. } = path;
    log::warn!("use len value={len}");
    let repo_handle = (state.repositories.write().unwrap())
        .get_config(repo_spec)
        .ok_or_else(|| "missing config for repository".to_string())?;
    let mut repository = repo_handle.fetch();
    log::warn!("done cloning {}", repository.spec);
    let commits = (state.repositories.write().unwrap())
        .pre_process_with_limit(&mut repository, "", &commit, 4)
        .map_err(|e| e.to_string())?;
    let prepare_time = now.elapsed().as_secs_f64();
    let now = Instant::now();
    log::warn!(
        "done construction of {commits:?} in {}",
        repository.spec.user()
    );
    let src_oid = commits[0];
    let dst_oid = commits[1];
    let diff = diffing::diff(state, &repository, dst_oid, src_oid).map_err(|e| e.to_string())?;
    dbg!(diff.moves.len());
    dbg!(diff.deletes.len());
    let focuses = diff.focuses;
    let inserts = &diff.inserts;
    let deletes = &diff.deletes;
    let moves = diff.moves;
    let examples = focuses
        .iter()
        .map(|(l, r)| {
            let after = LocalPieceOfCode::from_position(
                &l.0,
                l.1.iter().map(|x| *x as usize).collect(),
                vec![],
            );
            let after = after.globalize(&repository.spec, src_oid);
            let before = LocalPieceOfCode::from_position(
                &r.0,
                r.1.iter().map(|x| *x as usize).collect(),
                vec![],
            );
            let before = before.globalize(&repository.spec, dst_oid);
            let deletes = deletes
                .iter()
                .filter(|x| x.0.file() == l.0.file())
                .map(|x| x.0.range())
                .collect();
            let inserts = inserts
                .iter()
                .filter(|x| x.0.file() == r.0.file())
                .map(|x| x.0.range())
                .collect();
            let moves = moves
                .iter()
                .filter(|(y, x)| x.0.file() == l.0.file() && y.0.file() == r.0.file())
                .map(|(y, x)| (x.0.range(), y.0.range()))
                .collect();
            ExamplesValue {
                before,
                after,
                deletes,
                inserts,
                moves,
            }
        })
        .collect();
    let moves: Vec<_> = moves
        .into_iter()
        .map(|(l, r)| {
            let after = l.1.iter().map(|x| *x as usize).collect();
            let after = LocalPieceOfCode::from_position(&l.0, after, vec![]);
            let before = r.1.iter().map(|x| *x as usize).collect();
            let before = LocalPieceOfCode::from_position(&r.0, before, vec![]);
            (
                after.globalize(&repository.spec, src_oid),
                before.globalize(&repository.spec, dst_oid),
            )
        })
        .collect();
    let diff_time = now.elapsed().as_secs_f64();
    log::warn!(
        "done computing diff on {commits:?} in {}: found {} focuses, {} moves, {} inserts, and {} deletes",
        repository.spec.user(),
        focuses.len(),
        moves.len(),
        inserts.len(),
        deletes.len(),
    );
    Ok(ExamplesResults {
        examples,
        moves,
        prepare_time,
        search_time: diff_time,
    })
}

pub(crate) struct Diff {
    // actions: Option<ActionsVec<SimpleAction<LabelIdentifier, CompressedTreePath<Idx>, NodeIdentifier>>>,
    focuses: Vec<(Pos, Pos)>,
    deletes: Vec<Pos>,
    inserts: Vec<Pos>,
    moves: Vec<(Pos, Pos)>,
}

mod poset_exploration;

pub(crate) type Pos = (
    hyperast::position::file_and_offset::Position<std::path::PathBuf, usize>,
    Vec<Idx>,
);

#[cfg(not(feature = "lattice"))]
pub(crate) mod lattice {
    use super::Idx;
    use hyperast::position::StructuralPosition;
    use hyperast_gen_ts_tsquery::code2query::QueryLattice;
    use hyperast_vcs_git::TStore;
    type IdN = hyperast::store::nodes::legion::NodeIdentifier;
    type HAST = hyperast::store::SimpleStores<TStore>;
    pub type G = Vec<()>;
    pub fn prep<P: PartialEq, F: Fn(&P) -> String>(_lattice: &QueryLattice<&P>, _f: F) -> G {
        vec![]
    }
}
#[cfg(feature = "lattice")]
pub(crate) mod lattice;

#[cfg(test)]
mod tests {
    use super::*;

    #[ignore]
    #[test_log::test]
    fn test_finding_smells_gson_try_fail_catch()
    -> std::result::Result<(), Box<dyn std::error::Error>> {
        let user = "Marcono1234";
        let name = "gson";
        let config = hyperast_vcs_git::processing::RepoConfig::JavaMaven;
        let commit = "3d241ca0a6435cbf1fa1cdaed2af8480b99fecde";
        let language = "Java";
        let param = Param {
            user: user.to_string(),
            name: name.to_string(),
            commit: commit.to_string(),
            len: 1,
        };
        let repo_spec = param.repo();

        let state = crate::AppState::default();
        (state.repositories.write().unwrap()).register_config(repo_spec.clone(), config);
        let state = std::sync::Arc::new(state);

        let examples = smells_ex_from_diffs(state.clone(), param.clone())?;

        // ;(_
        // ;    (named_node
        // ;        (identifier) (#EQ? "expression_statement")
        // ;    ) @rm
        // ;)
        // ;(_
        // ;    (predicate) @rm
        // ;)
        // (_
        // ;    (named_node
        // ;        (identifier) (#EQ? "statement")
        // ;    ) @rm
        // ;)
        // ;(_
        // ;    (named_node
        // ;        (identifier) (#EQ? "argument_list")
        // ;    ) @rm
        // ;)
        // ;(_
        // ;    (named_node
        // ;        (identifier) (#EQ? "catch_formal_parameter")
        // ;    ) @rm
        // ;)
        // ;(named_node
        // ;    (identifier)
        // ;    (named_node) .
        // ;    (predicate
        // ;        (identifier) (#EQ? "EQ")
        // ;        (parameters
        // ;            (string) @label
        // ;        )
        // ;    ) @pred
        // ;)
        // (named_node
        //     (identifier) (#EQ? "expression_statement")
        //     (named_node
        //         (identifier) (#EQ? "method_invocation")
        //     ) @rm
        // )
        // (named_node
        //     (identifier) (#EQ? "catch_type")
        // ) @rm.all.full
        // (named_node
        //     (identifier) (#EQ? "catch_formal_parameter")
        // ) @rm.all.full
        let examples = Examples {
            simple_matching: true,
            prepro_matching: true,
            meta_gen: META_GEN.into(),
            meta_simp: META_SIMP.into(),
            examples: examples.examples,
        };
        let res = smells(examples, state, param)?;
        // for x in res.bad {
        //     eprintln!();
        //     eprintln!("{}", x.query);
        // }
        Ok(())
    }
}

const META_GEN: &str = r#"
    [
        "{" ";" "." "try" "(" ")" "}" "catch" "import"
        (line_comment) (block_comment)
    ] @skip
    (type_identifier) @label
    (identifier) @label
    (_literal) @abstract
"#;

const META_SIMP: &str = r#"(named_node
        (identifier) (#EQ? "try_statement")
    ) @uniq
    (named_node
        (identifier) (#EQ? "method_invocation")
    ) @need
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
        (identifier) (#EQ? "unary_expression")
        (named_node
            (identifier) .
        )
    ) @rm
    (named_node
        (identifier) (#EQ? "field_access")
        (named_node
        )
        (named_node
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
            (predicate
                (identifier) (#EQ? "EQ")
                (parameters
                    (string)
                )
            )
        ) .
    ) @rm
    (named_node
        (identifier) (#EQ? "method_invocation")
        (named_node
            (identifier) (#EQ? "identifier")
            (predicate
                (identifier) (#EQ? "EQ")
                (parameters
                    (string)
                )
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
            (predicate
                (identifier) (#EQ? "EQ")
                (parameters
                    (string)
                )
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
            (identifier) (#EQ? "identifier") .
            (predicate
                (identifier) (#EQ? "EQ")
                (parameters
                    (string) @label
                )
            ) @pred
        )
    ) @rm.all
    (named_node
        (identifier) (#EQ? "method_invocation")
        (named_node
            (identifier) (#EQ? "identifier") .
            (predicate
                (identifier) (#EQ? "EQ")
                (parameters
                    (string)
                )
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

#[cfg(feature = "lattice")]
/// experimenting with webgraph and graph compression
mod graph_compression {
    use num::ToPrimitive;
    use petgraph::graph::{Graph, NodeIndex};
    use petgraph::visit::{EdgeCount, EdgeRef, NodeIndexable};

    // use webgraph::graphs::ImmutableGraph;
    use webgraph::graphs::arc_list_graph::ArcListGraph;
    use webgraph::graphs::vec_graph::{LabeledVecGraph, VecGraph}; // VecGraph is an in-memory implementation

    pub fn petgraph_to_webgraph(g: &Graph<(), ()>) -> VecGraph {
        // Create a flat list of edges (arcs)
        let mut arcs = Vec::new();
        for edge in g.edge_references() {
            let from = edge.source().index();
            let to = edge.target().index();
            arcs.push((from, to));
        }

        // Build the WebGraph VecGraph from the list of arcs
        VecGraph::from_arcs(arcs)
    }

    // fn webgraph_to_petgraph(g: &VecGraph) -> Graph<(), ()> {
    //     let mut pg = Graph::<(), ()>::new();

    //     // Add all nodes
    //     let nodes: Vec<_> = (0..g.num_nodes()).map(|_| pg.add_node(())).collect();

    //     // For each source node, iterate its successors (neighbors)
    //     for src in 0..g.num_nodes() {
    //         for &dst in g.successors(src as u32).unwrap_or(&[]) {
    //             pg.add_edge(nodes[src], nodes[dst as usize], ());
    //         }
    //     }

    //     pg
    // }

    #[test]
    fn test() {
        // Create a simple Petgraph graph
        let mut g = Graph::<(), ()>::new();
        let a = g.add_node(());
        let b = g.add_node(());
        let c = g.add_node(());
        g.add_edge(a, b, ());
        g.add_edge(b, c, ());

        // Convert to Webgraph
        let webg = petgraph_to_webgraph(&g);
        use webgraph::traits::SequentialLabeling;
        println!("Webgraph has {} nodes", webg.num_nodes());
    }

    use petgraph::csr::Csr;
    use petgraph::{Directed, EdgeType};
    use webgraph::graphs::csr_graph::{CompressedCsrGraph, CsrGraph};
    use webgraph::labels::{Left, LeftIterator};
    use webgraph::traits::{
        NodeLabelsLender, RandomAccessLabeling, SequentialGraph, SequentialLabeling,
        SortedIterator, UnitLabelGraph,
    };

    pub fn graph_to_csr<N: Clone, E: Clone>(g: &Graph<N, E, Directed>) -> Csr<N, E, Directed> {
        let node_count = g.node_count();
        // let mut csr: Csr<(), _, Directed> = Csr::with_nodes(node_count);
        let mut csr: Csr<_, _, Directed> = Csr::new();
        g.node_weights().for_each(|n| {
            csr.add_node(n.clone());
        });

        // Add edges — Csr expects `(NodeIndex, EdgeWeight)` tuples
        for edge in g.edge_references() {
            eprintln!(
                "{} {}",
                edge.target().index() as u32,
                edge.source().index() as u32,
            );
            let added = csr.add_edge(
                // reversed
                edge.target().index() as u32,
                edge.source().index() as u32,
                edge.weight().clone(),
            );
            assert!(
                added,
                "{} {}",
                edge.target().index() as u32,
                edge.source().index() as u32,
            );
        }
        csr
    }

    // use webgraph::graphs::arc_list_graph::SeqArcListGraph; // needed for from_arcs_sorted()
    // use webgraph::graphs::csr_graph::CompressedCsrGraph;
    // use webgraph::traits::SequentialGraph;

    // fn arclist_to_compressed(arcs: &[(u32, u32)]) -> CompressedCsrGraph {
    //     // Wrap arcs in SeqArcListGraph so it implements SequentialGraph
    //     let seq = ArcListGraph::new(arcs.to_vec());

    //     let num_arcs = arcs.len();
    //     CompressedCsrGraph::from_sequential_graph(seq, num_arcs)
    // }

    #[test]
    fn test_csr() {
        // Create a simple Petgraph graph
        let mut g = Graph::<i8, u8>::new();
        for _ in 0..10000 {
            let a = g.add_node(-42);
            let b = g.add_node(-3);
            let c = g.add_node(-5);
            g.add_edge(a, b, 21);
            g.add_edge(b, c, 1);
            let d = g.add_node(-2);
            g.add_edge(a, d, 11);
            g.add_edge(d, c, 12);
        }
        let csr = graph_to_csr(&g);

        let seq =
            LabeledVecGraph::from_arcs((0..csr.node_count().to_u32().unwrap()).flat_map(|u| {
                use petgraph::adj::IndexType;
                use petgraph::visit::IntoNeighbors;
                csr.edges(u).map(|e| {
                    (
                        (e.source().to_usize().unwrap(), e.target() as usize),
                        e.weight().clone() as i8,
                    )
                })
            }));
        use webgraph::traits::SequentialLabeling;
        println!(
            "non compressed webgraph has {} nodes and {} arcs",
            seq.num_nodes(),
            seq.num_arcs(),
        );

        println!("seq: {}", serde_json::to_string(&seq).unwrap().len()); // not very useful yet
        println!(
            "csr nodes: {}",
            serde_json::to_string(
                &(0..csr.node_count().to_u32().unwrap())
                    .map(|i| csr[i])
                    .collect::<Vec<_>>()
            )
            .unwrap()
            .len()
        );
        println!(
            "csr edges: {}",
            serde_json::to_string(
                &(0..csr.node_count().to_u32().unwrap())
                    .map(|u| {
                        use petgraph::adj::IndexType;
                        use petgraph::visit::IntoNeighbors;
                        csr.edges(u)
                            .map(|e| (e.target(), e.weight().clone() as i8))
                            .collect::<Vec<_>>()
                    })
                    .collect::<Vec<_>>()
            )
            .unwrap()
            .len()
        );
        println!(
            "csr edges labels: {}",
            serde_json::to_string(
                &(0..csr.node_count().to_u32().unwrap())
                    .map(|u| {
                        use petgraph::adj::IndexType;
                        use petgraph::visit::IntoNeighbors;
                        csr.edges(u)
                            .map(|e| e.weight().clone() as i8)
                            .collect::<Vec<_>>()
                    })
                    .collect::<Vec<_>>()
            )
            .unwrap()
            .len()
        );
        println!(
            "csr edges no lab: {}",
            serde_json::to_string(
                &(0..csr.node_count().to_u32().unwrap())
                    .map(|u| {
                        use petgraph::adj::IndexType;
                        use petgraph::visit::IntoNeighbors;
                        csr.edges(u).map(|e| e.target()).collect::<Vec<_>>()
                    })
                    .collect::<Vec<_>>()
            )
            .unwrap()
            .len()
        );
        use lender::{IntoLender, Lender};
        let seq = Left(seq);
        let mut lender = seq.into_lender();
        // while let Some((x, i)) = lender.next() {
        //     let s = i.into_iter().collect::<Vec<_>>();
        //     println!("{:?} {:?}", x, s);
        //     // assert_eq!(p.labels(x).into_iter().collect::<Vec<_>>(), s);
        //     // assert_eq!(v.labels(x).collect::<Vec<_>>(), s);
        // }
        let webg = CompressedCsrGraph::try_from_graph(&seq).unwrap();

        println!(
            "compressed webgraph has {} nodes and {} arcs",
            webg.num_nodes(),
            webg.num_arcs()
        );

        println!("Csr EF: {}", serde_json::to_string(&webg).unwrap().len());
    }

    pub fn compress(g: petgraph::Graph<crate::utils::IdN, ()>) {
        let csr = graph_to_csr(&g);

        let seq =
            LabeledVecGraph::from_arcs((0..csr.node_count().to_u32().unwrap()).flat_map(|u| {
                use petgraph::adj::IndexType;
                use petgraph::visit::IntoNeighbors;
                csr.edges(u)
                    .map(|e| ((e.source().to_usize().unwrap(), e.target() as usize), ()))
            }));
        use webgraph::traits::SequentialLabeling;
        println!(
            "non compressed webgraph has {} nodes and {} arcs",
            seq.num_nodes(),
            seq.num_arcs(),
        );

        // println!("seq: {}", serde_json::to_string(&seq).unwrap().len()); // not very useful yet
        println!(
            "csr nodes: {}",
            serde_json::to_string(
                &(0..csr.node_count().to_u32().unwrap())
                    .map(|i| {
                        let idn = csr[i];
                        let idn: u64 = unsafe { std::mem::transmute(idn) };
                        idn
                    })
                    .collect::<Vec<_>>()
            )
            .unwrap()
            .len()
        );
        println!(
            "csr edges: {}",
            serde_json::to_string(
                &(0..csr.node_count().to_u32().unwrap())
                    .map(|u| {
                        use petgraph::adj::IndexType;
                        use petgraph::visit::IntoNeighbors;
                        csr.edges(u).map(|e| (e.target(), ())).collect::<Vec<_>>()
                    })
                    .collect::<Vec<_>>()
            )
            .unwrap()
            .len()
        );
        println!(
            "csr edges labels: {}",
            serde_json::to_string(
                &(0..csr.node_count().to_u32().unwrap())
                    .map(|u| {
                        use petgraph::adj::IndexType;
                        use petgraph::visit::IntoNeighbors;
                        csr.edges(u).map(|e| ()).collect::<Vec<_>>()
                    })
                    .collect::<Vec<_>>()
            )
            .unwrap()
            .len()
        );
        println!(
            "csr edges no lab: {}",
            serde_json::to_string(
                &(0..csr.node_count().to_u32().unwrap())
                    .map(|u| {
                        use petgraph::adj::IndexType;
                        use petgraph::visit::IntoNeighbors;
                        csr.edges(u).map(|e| e.target()).collect::<Vec<_>>()
                    })
                    .collect::<Vec<_>>()
            )
            .unwrap()
            .len()
        );
        use lender::{IntoLender, Lender};
        let seq = Left(seq);
        let mut lender = seq.into_lender();
        // while let Some((x, i)) = lender.next() {
        //     let s = i.into_iter().collect::<Vec<_>>();
        //     println!("{:?} {:?}", x, s);
        //     // assert_eq!(p.labels(x).into_iter().collect::<Vec<_>>(), s);
        //     // assert_eq!(v.labels(x).collect::<Vec<_>>(), s);
        // }
        let webg = CompressedCsrGraph::try_from_graph(&seq).unwrap();

        println!(
            "compressed webgraph has {} nodes and {} arcs",
            webg.num_nodes(),
            webg.num_arcs()
        );

        println!("Csr EF: {}", serde_json::to_string(&webg).unwrap().len());
    }
}
