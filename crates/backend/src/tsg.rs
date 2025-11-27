use axum::Json;
use serde::{Deserialize, Serialize};
use std::{fmt::Debug, hash::Hash, ops::Deref, time::Instant};

use hyperast::position::structural_pos::PersistedNode;
use hyperast::store::nodes::legion::NodeStoreInner;
use hyperast::types::{HyperAST, HyperASTShared, LendT, NodeId, RoleStore};
use hyperast::types::{WithPrecompQueries, WithRoles, WithSerialization, WithStats};
use hyperast_vcs_git::{SimpleStores, preprocessed::child_at_path};

use tree_sitter_graph::execution::lazy::execute_stmt_lazy;
use tree_sitter_graph::graph::{NodeLending, NodesLending, QMatch, SyntaxNode, WithSynNodes};
use tree_sitter_graph::{GenQuery, MatchLending, MatchesLending, MyQueryMatch};

use crate::SharedState;

#[derive(Deserialize, Clone)]
pub struct Param {
    user: String,
    name: String,
    commit: String,
}

impl Param {
    pub fn repo(&self) -> hyperast_vcs_git::git::Repo {
        hyperast_vcs_git::git::Forge::Github.repo(&self.user, &self.name)
    }
}

#[derive(Deserialize, Clone)]
pub struct Content {
    pub language: String,
    pub query: String,
    pub commits: usize,
    pub path: Option<String>,
}

impl Content {
    fn language(&self) -> Result<tree_sitter::Language, QueryingError> {
        hyperast_vcs_git::resolve_language(&self.language)
            .ok_or_else(|| QueryingError::MissingLanguage(self.language.clone()))
    }
}

#[derive(Debug, Serialize, Clone)]
pub enum QueryingError {
    MissingLanguage(String),
    TsgParsing(String),
}

#[derive(Serialize)]
pub struct ComputeResults {
    pub prepare_time: f64,
    pub results: Vec<Result<ComputeResultIdentified, String>>,
}

#[derive(Serialize)]
pub struct ComputeResultIdentified {
    pub commit: String,
    #[serde(flatten)]
    pub inner: ComputeResult,
}

#[derive(Serialize)]
pub struct ComputeResult {
    pub compute_time: f64,
    pub result: serde_json::Value,
}

pub fn simple(
    query: Content,
    state: SharedState,
    path: Param,
) -> Result<Json<ComputeResults>, QueryingError> {
    let now = Instant::now();
    let repo_spec = path.repo();
    let Param { commit, .. } = path;
    let language = query.language()?;
    let Content {
        query,
        commits,
        path,
        ..
    } = query;
    let repo = (state.repositories.read().unwrap()).get_config(repo_spec.clone());
    let repo = match repo {
        Some(repo) => repo,
        None => {
            let configs = &mut state.repositories.write().unwrap();
            configs.register_config(
                repo_spec.clone(),
                hyperast_vcs_git::processing::RepoConfig::JavaMaven,
            );
            // let repo = (state.repositories.write().unwrap()).register_config_with_tsg();
            log::error!("missing config for {}", repo_spec);
            configs.get_config(repo_spec.clone()).unwrap()
        }
    };
    let mut repo = repo.fetch();
    log::warn!("done cloning {}", &repo.spec);
    let commits = (state.repositories.write().unwrap())
        .pre_process_with_limit(&mut repo, "", &commit, commits)
        .unwrap();
    let path = &path.unwrap_or_default();

    let tsg = {
        use hyperast_tsquery::hyperast_opt::tsg_impl::ExtendingStringQuery;
        type M<HAST> = QueryMatcher<HAST>;
        type ExtQ<HAST> = ExtendingStringQuery<M<HAST>, tree_sitter::Language>;

        let source: &str = &query;

        let mut file = tree_sitter_graph::ast::File::<M<_>>::new(language.clone());

        tree_sitter_graph::parser::Parser::<ExtQ<&_>>::new(source)
            .parse_into_file(&mut file)
            .map_err(|e| QueryingError::TsgParsing(e.to_string()))?;

        // TODO
        // let precomputeds: Box<dyn hyperast_tsquery::ArrayStr> =
        //     (state.repositories.read().unwrap())
        //         .get_precomp_query(repo.config, &lang_name)
        //         .map_or(Box::new([].as_slice()), |x| Box::new(x));
        // let query_source = ExtQ::new(language.clone(), precomputeds, source.len());
        // tree_sitter_graph::parser::Parser::<ExtQ>::with_ext(query_source, source)
        //     .parse_into_file(&mut file)
        //     .map_err(|e| QueryingError::TsgParsing(e.to_string()))?;

        use tree_sitter_graph::GenQuery;

        M::check(&mut file).unwrap();

        // QueryMatcher::<SimpleStores>::check(&mut file)
        //     .map_err(|e| QueryingError::TsgParsing(e.to_string()))?;
        file
    };
    let prepare_time = now.elapsed().as_secs_f64();
    log::info!("done construction of {commits:?} in  {}", repo.spec);
    let mut results = vec![];
    for commit_oid in &commits {
        let result = simple_aux(&state, &repo, commit_oid, &query, &tsg, path)
            .map(|inner| ComputeResultIdentified {
                commit: commit_oid.to_string(),
                inner,
            })
            .map_err(|err| format!("{:?}", err));
        results.push(result);
    }
    log::info!("done querying of {commits:?} in  {}", repo.spec);
    Ok(Json(ComputeResults {
        prepare_time,
        results,
    }))
}

use hyperast_tsquery::CaptureId;
use hyperast_tsquery::hyperast_opt::tsg_impl::QueryMatcher;
use hyperast_tsquery::{Cursor, hyperast_opt::tsg_impl::Node};
use tree_sitter_graph::Variables as Globals; // I prefer to avoid the aliasing done at the root of tsg
use tree_sitter_graph::functions::Functions;

fn simple_aux(
    state: &crate::AppState,
    repo: &hyperast_vcs_git::processing::ConfiguredRepo2,
    commit_oid: &hyperast_vcs_git::git::Oid,
    query: &str,
    tsg: &tree_sitter_graph::ast::File<QueryMatcher<&SimpleStores>>,
    path: &str,
) -> Result<ComputeResult, QueryingError> {
    let cancellation_flag = &tree_sitter_graph::NoCancellation;
    type IdN = hyperast::store::nodes::legion::NodeIdentifier;
    type Idx = u16;

    use hyperast::position::structural_pos::CursorWithPersistence as Pos;
    use hyperast_tsquery::hyperast_opt::TreeCursorNoRef as Cursor;
    use hyperast_tsquery::hyperast_opt::tsg_impl::MyQMatch as QMatch;
    use tree_sitter_graph::graph;

    let now = Instant::now();
    let mut globals = Globals::new();

    let repositories = state.repositories.read().unwrap();
    let commit = repositories.get_commit(&repo.config, commit_oid).unwrap();
    let code = commit.ast_root;
    let stores = &repositories.processor.main_stores;
    let code = child_at_path(stores, code, path.split('/')).unwrap();

    let mut graph = Graph::new(stores);

    init_globals(&mut globals, &mut graph);

    let mut functions = Functions::stdlib();
    // TODO add it back, or put it behind a feature
    // tree_sitter_stack_graphs::functions::add_path_functions(&mut functions);
    let mut config = configure(&globals, &functions);

    let cursor = Cursor::new(stores, Pos::new(code));

    // let mut cursor = Default::default();
    let q = tsg.query.as_ref().unwrap();
    // let mut matches = query.matches(&mut cursor, &tree);
    let mut matches = q.query.matches(cursor); // Easier this way

    // NOTE I introduced Ctx to have a more fine grained control
    // and to necessitate less complex generic bounds that can be hard to debug, esp. nested lending constraints.
    let mut ctx = tree_sitter_graph::execution::Ctx::new();

    loop {
        let Some(mat) = matches.next() else {
            break;
        };
        let stanza = &tsg.stanzas[mat.pattern_index.to_usize()];
        assert_ne!(
            u32::MAX,
            stanza.full_match_file_capture_index,
            "uninitialized capture index" // NOTE QueryMatcher::check is in charge of that, have you called it, is it doing its job ?
        );

        let cid = stanza.full_match_file_capture_index.into();

        let mat = QMatch {
            stores,
            qm: mat,
            i: cid,
        };
        let node = (mat.qm.nodes_for_capture_index(cid))
            .next()
            .expect("missing capture for full match");

        ctx.clear();

        let current_regex_captures = vec![];
        for statement in &stanza.statements {
            dbg!();
            use tree_sitter_graph::execution::error::StatementContext;
            let error_context = StatementContext::raw(
                &statement,
                stanza.range.start,
                tree_sitter_graph::Location { row: 0, column: 0 },
                node.kind().to_string(),
            );
            let graph: &mut Graph<
                &hyperast::store::SimpleStores<hyperast_vcs_git::TStore>,
                PersistedNode<IdN, Idx>,
            > = &mut graph;
            let r = ctx.execplicit::<_, _, _, _>(
                &mat,
                graph,
                &tsg.inherited_variables,
                cancellation_flag,
                cid,
                &tsg.shorthands,
                &config,
                &current_regex_captures,
                &statement,
                error_context,
            );
            if let Err(err) = r {
                let source_path = std::path::Path::new(&"");
                let tsg_path = std::path::Path::new(&"");
                eprintln!("{}", err.display_pretty(source_path, "", tsg_path, query));
            }
        }
    }
    if let Err(err) = ctx.eval(
        &mut graph,
        config.functions,
        &tsg.inherited_variables,
        cancellation_flag,
    ) {
        // println!("{}", graph.pretty_print()); // TODO pub it back
        let source_path = std::path::Path::new(&"");
        let tsg_path = std::path::Path::new(&"");
        eprintln!("{}", err.display_pretty(source_path, "", tsg_path, query));
    };
    dbg!();

    let result = serde_json::to_value(graph).unwrap();
    let compute_time = now.elapsed().as_secs_f64();
    Ok(ComputeResult {
        result,
        compute_time,
    })
}

static DEBUG_ATTR_PREFIX: &str = "debug_";
pub static ROOT_NODE_VAR: &str = "ROOT_NODE";
/// The name of the file path global variable
pub const FILE_PATH_VAR: &str = "FILE_PATH";
static JUMP_TO_SCOPE_NODE_VAR: &str = "JUMP_TO_SCOPE_NODE";
static FILE_NAME: &str = "a/b/AAA.java";

// use tree_sitter_graph::graph::Graph;
fn configure<'a, 'b, 'g, SNode, HAST>(
    globals: &'b Globals<'g>,
    functions: &'a Functions<Graph<SNode, HAST>>,
) -> tree_sitter_graph::ExecutionConfig<'a, 'g, 'b, Graph<SNode, HAST>> {
    let config = tree_sitter_graph::ExecutionConfig::new(functions, globals)
        .lazy(true)
        .debug_attributes(
            [DEBUG_ATTR_PREFIX, "tsg_location"].concat().as_str().into(),
            [DEBUG_ATTR_PREFIX, "tsg_variable"].concat().as_str().into(),
            [DEBUG_ATTR_PREFIX, "tsg_match_node"]
                .concat()
                .as_str()
                .into(),
        );
    config
}

// NOTE syntax nodes are not needed here,
// extracting WithNodes would simplify the bound on graph
fn init_globals(globals: &mut Globals, graph: &mut impl WithSynNodes) {
    globals
        .add(ROOT_NODE_VAR.into(), graph.add_graph_node().into())
        .expect("Failed to set ROOT_NODE");
    globals
        .add(FILE_PATH_VAR.into(), FILE_NAME.into())
        .expect("Failed to set FILE_PATH");
    globals
        .add(JUMP_TO_SCOPE_NODE_VAR.into(), graph.add_graph_node().into())
        .expect("Failed to set JUMP_TO_SCOPE_NODE");
}

use tree_sitter_graph::graph::GraphNode;
use tree_sitter_graph::graph::GraphNodeRef;
use tree_sitter_graph::graph::SimpleNode;
use tree_sitter_graph::graph::SyntaxNodeID;
use tree_sitter_graph::graph::SyntaxNodeRef;
use tree_sitter_graph::graph::WithAttrs;
use tree_sitter_graph::graph::WithOutGoingEdges;

/// A graph produced by executing a graph DSL file.  Graphs include an implicit lifetime on `S` to ensure
/// that they don't outlive the syntax tree that they are generated from.
pub struct Graph<HAST, S, N = GraphNode> {
    pub(crate) syntax_nodes: std::collections::HashMap<SyntaxNodeID, S>,
    graph_nodes: Vec<N>,
    hast: HAST,
}

impl<'a, N, HAST: HyperAST + Copy> NodeLending<'a>
    for Graph<HAST, PersistedNode<HAST::IdN, HAST::Idx>, N>
where
    HAST::IdN: Copy,
    HAST::IdN: Debug + Hash,
    HAST::Idx: Hash,
    HAST::IdN: Copy + Hash + Debug,
    HAST::Idx: Copy + Hash,
    for<'t> LendT<'t, HAST>: WithSerialization + WithStats,
    HAST::IdN: NodeId<IdN = HAST::IdN>,
    HAST::TS: RoleStore,
    for<'t> LendT<'t, HAST>: WithRoles + WithPrecompQueries,
{
    type SNode = Node<HAST>;
}

impl<HAST: HyperAST + Copy, N: WithAttrs + Default + WithOutGoingEdges> WithSynNodes
    for Graph<HAST, PersistedNode<HAST::IdN, HAST::Idx>, N>
where
    HAST::IdN: Copy + Hash + Debug,
    HAST::Idx: Copy + Hash,
    for<'t> LendT<'t, HAST>: WithSerialization + WithStats,
    HAST::IdN: NodeId<IdN = HAST::IdN>,
    HAST::TS: RoleStore,
    for<'t> LendT<'t, HAST>: WithRoles + WithPrecompQueries,
{
    type Node = N;

    fn node(&self, r: SyntaxNodeRef) -> Option<<Self as NodeLending<'_>>::SNode> {
        self.syntax_nodes.get(&r.index).map(|x| Node {
            stores: self.hast,
            pos: x.clone(),
        })
    }

    fn add_graph_node(&mut self) -> GraphNodeRef {
        self.add_graph_node()
    }

    fn add_syntax_node(&mut self, node: <Self as NodeLending<'_>>::SNode) -> SyntaxNodeRef {
        use tree_sitter_graph::graph::SyntaxNode;
        let index = node.id() as SyntaxNodeID;
        let node_ref = SyntaxNodeRef {
            index,
            kind: SyntaxNode::kind(&node),
            position: node.start_position(),
        };

        self.syntax_nodes.entry(index).or_insert(node.pos.clone());
        node_ref
    }
}

impl<HAST: HyperAST, N> std::ops::Index<GraphNodeRef>
    for Graph<HAST, PersistedNode<HAST::IdN, HAST::Idx>, N>
{
    type Output = N;
    fn index(&self, index: GraphNodeRef) -> &N {
        &self.graph_nodes[index.0 as usize]
    }
}

impl<HAST: HyperAST, N> std::ops::IndexMut<GraphNodeRef>
    for Graph<HAST, PersistedNode<HAST::IdN, HAST::Idx>, N>
{
    fn index_mut(&mut self, index: GraphNodeRef) -> &mut N {
        &mut self.graph_nodes[index.0 as usize]
    }
}

type GraphNodeID = u32;

impl<HAST: HyperAST, N: Default> Graph<HAST, PersistedNode<HAST::IdN, HAST::Idx>, N> {
    /// Adds a new graph node to the graph, returning a graph DSL reference to it.
    pub fn add_graph_node(&mut self) -> GraphNodeRef {
        let graph_node = N::default();
        let index = self.graph_nodes.len() as GraphNodeID;
        self.graph_nodes.push(graph_node);
        GraphNodeRef(index)
    }

    // Returns an iterator of references to all of the nodes in the graph.
    pub fn iter_nodes(&self) -> impl Iterator<Item = GraphNodeRef> {
        (0..self.graph_nodes.len() as u32).map(GraphNodeRef)
    }

    // Returns the number of nodes in the graph.
    pub fn node_count(&self) -> usize {
        self.graph_nodes.len()
    }

    fn new(hast: HAST) -> Self {
        Self {
            syntax_nodes: Default::default(),
            graph_nodes: Default::default(),
            hast,
        }
    }
}

impl<HAST: HyperAST + Copy, N: Default> Graph<HAST, PersistedNode<HAST::IdN, HAST::Idx>, N>
where
    HAST::IdN: Copy,
    HAST::IdN: Debug + Hash,
    HAST::Idx: Hash,
    HAST::IdN: Copy + Hash + Debug,
    HAST::Idx: Copy + Hash,
    for<'t> LendT<'t, HAST>: WithSerialization + WithStats,
    HAST::IdN: NodeId<IdN = HAST::IdN>,
    HAST::TS: RoleStore,
    for<'t> LendT<'t, HAST>: WithRoles + WithPrecompQueries,
{
    /// Adds a syntax node to the graph, returning a graph DSL reference to it.
    ///
    /// The graph won't contain _every_ syntax node in the parsed syntax tree; it will only contain
    /// those nodes that are referenced at some point during the execution of the graph DSL file.
    pub fn add_syntax_node(&mut self, node: Node<HAST>) -> SyntaxNodeRef {
        let index = node.id() as SyntaxNodeID;
        let node_ref = SyntaxNodeRef {
            index,
            kind: node.kind(),
            position: node.start_position(),
        };
        self.syntax_nodes.entry(index).or_insert(node.pos.clone());
        node_ref
    }
}

impl<HAST, S> Serialize for Graph<HAST, S> {
    fn serialize<Ser: serde::Serializer>(&self, serializer: Ser) -> Result<Ser::Ok, Ser::Error> {
        use serde::ser::SerializeSeq;
        let mut seq = serializer.serialize_seq(Some(self.graph_nodes.len()))?;
        use tree_sitter_graph::graph::IntoIndexedSerialize;
        for (node_index, node) in self.graph_nodes.iter().enumerate() {
            seq.serialize_element(&node.with_index(node_index))?;
        }
        seq.end()
    }
}
