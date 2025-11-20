use std::{fmt::Debug, hash::Hash};

use hyperast::tree_gen;
use hyperast::types;
#[cfg(feature = "tsg")]
use hyperast::types::LendT;
use hyperast::types::{AstLending, ETypeStore, HyperASTShared, StoreRefAssoc};

use hyperast::position::structural_pos::StructuralPosition as Pos;

use super::stepped_query_imm;

type ImmFcts<HAST, Acc> = tree_sitter_graph::functions::Functions<
    tree_sitter_graph::graph::Graph<crate::stepped_query_imm::Node<HAST, Acc>>,
>;

pub struct PreparedOverlay<Q, O> {
    pub query: Option<Q>,
    pub overlayer: O,
    // NOTE I tried with ImmFcts<'a, TS, Acc>
    // and it does not help with the lifetime invariance of G in Functions
    pub functions: crate::ErazedFcts,
}

#[cfg(feature = "tsg")]
impl<HAST, Acc> tree_gen::More<HAST>
    for PreparedOverlay<
        &crate::Query,
        &tree_sitter_graph::ast::File<
            stepped_query_imm::QueryMatcher<<HAST as StoreRefAssoc>::S<'_>, &Acc>,
        >,
    >
where
    HAST: StoreRefAssoc,
    HAST::IdN: Copy + Hash + Debug,
    HAST::Idx: Hash,
    for<'t> LendT<'t, HAST>: types::WithSerialization + types::WithStats + types::WithRoles,
    HAST::TS: 'static + Clone,
    HAST::TS: ETypeStore<Ty2 = Acc::Type>,
    HAST::TS: types::RoleStore<IdF = u16, Role = types::Role>,
    Acc: tree_gen::WithRole<types::Role> + tree_gen::WithChildren<HAST::IdN> + types::Typed,
    for<'acc> &'acc Acc: tree_gen::WithLabel<L = &'acc str>,
    HAST::IdN: types::NodeId<IdN = HAST::IdN>,
{
    type Acc = Acc;

    const ENABLED: bool = true;

    fn match_precomp_queries(
        &self,
        stores: <HAST as StoreRefAssoc>::S<'_>,
        acc: &Acc,
        label: Option<&str>,
    ) -> tree_gen::PrecompQueries {
        let Some(query) = self.query else {
            return Default::default();
        };
        // self.query.match_precomp_queries(stores, acc, label)
        if query.enabled_pattern_count() == 0 {
            return Default::default();
        }
        let pos = Pos::empty();
        let cursor = crate::cursor_on_unbuild::TreeCursor::new(stores, acc, label, pos);
        use crate::cursor_on_unbuild::Node as N;
        let mut qcursor: crate::QueryCursor<
            '_,
            _,
            N<HAST::S<'_>, &Acc, HAST::Idx, Pos<HAST::IdN, HAST::Idx>, &str>,
        > = query.matches_immediate(cursor); // TODO filter on height (and visibility?)
        let mut r = Default::default();
        loop {
            let Some(m) = qcursor._next_match() else {
                break;
            };
            assert!(m.pattern_index.to_usize() < 16);
            r |= 1 << m.pattern_index.to_usize() as u16;
        }
        r
    }
}

#[cfg(feature = "tsg")]
impl<HAST, Acc> tree_gen::Prepro<HAST>
    for PreparedOverlay<
        &crate::Query,
        &tree_sitter_graph::ast::File<
            stepped_query_imm::QueryMatcher<<HAST as StoreRefAssoc>::S<'_>, &Acc>,
        >,
    >
where
    HAST::TS: ETypeStore,
    HAST: StoreRefAssoc,
    HAST::IdN: Copy + Hash + Debug,
    HAST::Idx: Hash,
    for<'t> LendT<'t, HAST>: types::WithSerialization + types::WithStats + types::WithRoles,
    HAST::TS: 'static + Clone,
    HAST::TS: ETypeStore<Ty2 = Acc::Type>,
    HAST::TS: types::RoleStore<IdF = u16, Role = types::Role>,
    Acc: tree_gen::WithRole<types::Role> + tree_gen::WithChildren<HAST::IdN> + types::Typed,
    for<'acc> &'acc Acc: tree_gen::WithLabel<L = &'acc str>,
    HAST::IdN: types::NodeId<IdN = HAST::IdN>,
{
    const USING: bool = false;
    type Scope = hyperast::scripting::Acc;

    fn preprocessing(
        &self,
        _ty: <HAST::TS as ETypeStore>::Ty2,
    ) -> Result<Self::Scope, <Self::Scope as hyperast::scripting::Scriptable>::Error> {
        unimplemented!()
    }

    fn scripts(&self) -> &<Self::Scope as hyperast::scripting::Scriptable>::Scripts {
        unimplemented!()
    }
}

#[cfg(feature = "tsg")]
impl<HAST, Acc> tree_gen::PreproTSG<HAST>
    for PreparedOverlay<
        &crate::Query,
        &tree_sitter_graph::ast::File<
            stepped_query_imm::QueryMatcher<<HAST as StoreRefAssoc>::S<'_>, &Acc>,
        >,
    >
where
    HAST: StoreRefAssoc,
    HAST::IdN: 'static + Copy + Hash + Debug,
    HAST::Idx: 'static + Hash,
    for<'t> LendT<'t, HAST>: types::WithSerialization + types::WithStats + types::WithRoles,
    HAST::TS: 'static + Clone,
    HAST::TS: ETypeStore<Ty2 = Acc::Type> + types::TypeStore,
    HAST::TS: types::RoleStore<IdF = u16, Role = types::Role>,
    Acc: 'static
        + tree_gen::WithRole<types::Role>
        + tree_gen::WithChildren<HAST::IdN>
        + types::Typed,
    for<'acc> &'acc Acc: tree_gen::WithLabel<L = &'acc str>,
    HAST::IdN: types::NodeId<IdN = HAST::IdN>,
{
    const GRAPHING: bool = true;
    // TODO remove the 'static and other contraints, they add unnecessary unsafes
    // there is probably something to do with spliting GenQuery and the different execs to avoid both
    // - holding graph as mutable to often
    // - bubling the mutability invariant from graph to HAST... (very bad)
    fn compute_tsg(
        &self,
        stores: <HAST as StoreRefAssoc>::S<'_>,
        acc: &Acc,
        label: Option<&str>,
    ) -> Result<usize, String> {
        // NOTE I had to do a lot of unsafe magic :/
        use tree_sitter_graph::functions;
        use tree_sitter_graph::graph::Graph;
        let cancellation_flag = tree_sitter_graph::NoCancellation;
        let mut globals = tree_sitter_graph::Variables::new();
        let mut graph = tree_sitter_graph::graph::Graph::default();
        init_globals(&mut globals, &mut graph);

        // retrieve the custom functions
        // NOTE need the concrete type of the stores to instantiate
        // // I tried other approaches but nothing worked,
        // // we cannot afford to rebuild the set each call,
        // // it cannot be passed easily as reference due to invariance over G (the fcts modify G)
        // let functions = std::ops::Deref::deref(&self.functions);
        // // SAFETY: assumes provided self.functions is the right one
        // let functions: &ImmFcts<<HAST as StoreRefAssoc>::S<'_>, &Acc> =
        //     unsafe { downcast_ref_unchecked(functions) };
        // pub unsafe fn downcast_ref_unchecked<T>(s: &dyn crate::Opaque) -> &T {
        //     // SAFETY: caller guarantees that T is the correct type
        //     unsafe { &*(s as *const dyn crate::Opaque as *const T) }
        // }

        let functions = self
            .functions
            .downcast_fcts::<crate::ImmGraph<<HAST as StoreRefAssoc>::S<'_>, &Acc>>();

        // // alternatively
        // let mut functions = &tree_sitter_graph::functions::Functions::stdlib();

        // tree_sitter_stack_graphs::functions::add_path_functions(&mut functions);

        let mut config = configure(&globals, functions);

        let pos = Pos::empty();
        let tree = stepped_query_imm::Node::new(stores, acc, label, pos);

        // NOTE could not use the execute_lazy_into due to limitations with type checks (needed some transmutes)
        // ORI: self.overlayer.execute_lazy_into2(&mut graph, tree, &config, &cancellation_flag).unwrap();
        // {
        let mut ctx = tree_sitter_graph::execution::Ctx::new();

        let mut cursor = vec![];
        // NOTE could not find a way to make it type check without inlining
        // ORI: let mut matches = this.query.matches(&mut cursor, tree);
        let mut matches = {
            let q: &stepped_query_imm::QueryMatcher<_, &Acc> =
                self.overlayer.query.as_ref().unwrap();
            // SAFETY: Fine for now, I don't know hot to make it work without bypassing 80% of tsg
            let q: &stepped_query_imm::QueryMatcher<_, &Acc> = unsafe { std::mem::transmute(q) };
            let cursor = &mut cursor;
            let qm = self.overlayer.query.as_ref().unwrap();
            let matchs = qm.query.matches_immediate(tree.clone());
            let node = tree.clone();
            stepped_query_imm::MyQMatches::<_, _, _> {
                q,
                cursor,
                matchs,
                node,
            }
        };
        let graph = &mut graph;
        loop {
            // NOTE needed to make a transmute to type check
            // ORI: ... matches.next() ...
            let mat: stepped_query_imm::MyQMatch<_, &Acc> = {
                let Some(mat) = matches.next() else { break };
                let qm: crate::QueryMatch<
                    stepped_query_imm::Node<
                        <HAST as StoreRefAssoc>::S<'_>,
                        &Acc,
                        HAST::Idx,
                        Pos<HAST::IdN, HAST::Idx>,
                        &str,
                    >,
                > = unsafe { std::mem::transmute(mat.qm) };
                stepped_query_imm::MyQMatch {
                    stores: tree.0.stores,
                    b: mat.b,
                    qm,
                    i: mat.i,
                }
            };
            use tree_sitter_graph::graph::QMatch;
            let stanza = &self.overlayer.stanzas[mat.pattern_index()];
            // NOTE could not type check it either
            // ORI: stanza.execute_lazy2(
            {
                let inherited_variables = &self.overlayer.inherited_variables;
                let shorthands = &self.overlayer.shorthands;
                let mat = &mat;
                let config = &mut config;
                let cancellation_flag = &cancellation_flag;
                let current_regex_captures = vec![];
                ctx.clear();
                let node = mat
                    .nodes_for_capture_indexi(stanza.full_match_file_capture_index.into())
                    .expect("missing capture for full match");
                log::error!("{:?}", node.0.pos);
                // debug!("match {:?} at {}", node, self.range.start);
                // trace!("{{");
                for statement in &stanza.statements {
                    // NOTE could not properly get the source location, just use a zeroed location
                    // ORI: let error_context = StatementContext::new(...
                    let error_context = {
                        let source_node = &node;
                        // use crate::graph::SyntaxNode;
                        // let source_location: Location::from(source_node.start_position()), // TODO make a better location for hyperast;
                        let source_location = tree_sitter_graph::Location { row: 0, column: 0 };
                        let kind = source_node.0.kind();
                        use tree_sitter_graph::execution::error::StatementContext;
                        StatementContext::raw(
                            &statement,
                            stanza.range.start,
                            source_location,
                            kind.to_string(),
                        )
                    };
                    let full_match_file_capture_index = stanza.full_match_file_capture_index.into();
                    if let Err(err) = ctx.exec(
                        graph,
                        inherited_variables,
                        cancellation_flag,
                        full_match_file_capture_index,
                        shorthands,
                        mat,
                        config,
                        &current_regex_captures,
                        statement,
                        error_context,
                    ) {
                        log::trace!("{}", graph.pretty_print());
                        let source_path = std::path::Path::new(&"");
                        let tsg_path = std::path::Path::new(&"");
                        log::error!("{}", err.display_pretty(source_path, "", tsg_path, ""));
                    }
                }
            };
        }

        if let Err(err) = ctx.eval(
            graph,
            functions,
            &self.overlayer.inherited_variables,
            &cancellation_flag,
        ) {
            log::trace!("{}", graph.pretty_print());
            let source_path = std::path::Path::new(&"");
            let tsg_path = std::path::Path::new(&"");
            log::error!("{}", err.display_pretty(source_path, "", tsg_path, ""));
        }
        // }

        // TODO properly return and use the graph (also handle the error propagation)
        if graph.node_count() > 2 {
            log::error!("curr kind {}", types::Typed::get_type(acc));
            let s = graph.to_json().unwrap();
            log::error!("graph: {}", s);
        }
        Ok(graph.node_count())
    }
}

// pub use tree_sitter_stack_graphs::functions::add_path_functions;

static DEBUG_ATTR_PREFIX: &str = "debug_";
pub static ROOT_NODE_VAR: &str = "ROOT_NODE";
/// The name of the file path global variable
pub const FILE_PATH_VAR: &str = "FILE_PATH";
static JUMP_TO_SCOPE_NODE_VAR: &str = "JUMP_TO_SCOPE_NODE";
static FILE_NAME: &str = "a/b/AAA.java";

#[cfg(feature = "tsg")]
pub fn configure<'a, 'g, 'b, G>(
    globals: &'b tree_sitter_graph::Variables<'g>,
    functions: &'a tree_sitter_graph::functions::Functions<G>,
) -> tree_sitter_graph::ExecutionConfig<'a, 'g, 'b, G> {
    let config = tree_sitter_graph::ExecutionConfig::new(functions, globals).lazy(true);
    if !cfg!(debug_assertions) {
        config.debug_attributes(
            [DEBUG_ATTR_PREFIX, "tsg_location"].concat().as_str().into(),
            [DEBUG_ATTR_PREFIX, "tsg_variable"].concat().as_str().into(),
            [DEBUG_ATTR_PREFIX, "tsg_match_node"]
                .concat()
                .as_str()
                .into(),
        )
    } else {
        config
    }
}

#[cfg(feature = "tsg")]
pub fn init_globals<Node>(
    globals: &mut tree_sitter_graph::Variables,
    graph: &mut tree_sitter_graph::graph::Graph<Node>,
) {
    // globals
    //     .add(ROOT_NODE_VAR.into(), graph.add_graph_node().into())
    //     .expect("Failed to set ROOT_NODE");
    // // globals
    // //     .add(FILE_PATH_VAR.into(), FILE_NAME.into())
    // //     .expect("Failed to set FILE_PATH");
    // globals
    //     .add(JUMP_TO_SCOPE_NODE_VAR.into(), graph.add_graph_node().into())
    //     .expect("Failed to set JUMP_TO_SCOPE_NODE");
}
