use std::borrow::Borrow;
use std::fmt::Display;

use hyperast::PrimInt;
use hyperast::position::PositionConverter;
use hyperast::position::StructuralPosition;
use hyperast::types::HyperAST;
use hyperast::types::HyperASTShared;
use hyperast::types::LendT;
use hyperast::types::WithSerialization;
use hyperast::types::WithStats;

use crate::code2query::QueryLattice;
#[cfg(feature = "lattice")]
use crate::lattice_graph::GroupedLattices;

#[cfg(feature = "lattice")]
use mermaid::Mermaid;

type IdNQ = hyperast::store::defaults::NodeIdentifier;

pub struct GroupedLatticesFmt<'a, 'b, HAST: HyperASTShared, Q, P> {
    pub(crate) lattice: &'a QueryLattice<P>,
    pub(crate) stores: &'a HAST,
    pub(crate) graphs: &'b GroupedLattices<Q>,
}

pub fn markdown<'a, 'b, HAST: HyperASTShared, Q, P>(
    graphs: &'b GroupedLattices<Q>,
    lattice: &'a QueryLattice<P>,
    stores: &'a HAST,
) -> GroupedLatticesFmt<'a, 'b, HAST, Q, P> {
    GroupedLatticesFmt {
        lattice,
        stores,
        graphs,
    }
}

pub(crate) const STYLE: &str = r###"<style>
details details {
    margin-left: 40px;
}
</style>"###;

impl<HAST: HyperAST, Q, P> Display for GroupedLatticesFmt<'_, '_, HAST, Q, P>
where
    HAST::IdN: std::fmt::Debug + Copy,
    HAST::IdN: hyperast::types::NodeId<IdN = HAST::IdN>,
    for<'t> LendT<'t, HAST>: WithSerialization + WithStats,
    Q: Clone + Borrow<IdNQ> + PQ,
    P: hyperast::position::position_accessors::SolvedPosition<HAST::IdN> + Copy + Eq,
    for<'t> (&'t HAST, P): PPP,
{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        use std::fmt::Formatter as Fmt;

        use crate::lattice_graph::LatticeStats;
        let GroupedLatticesFmt {
            lattice,
            stores,
            graphs: GroupedLattices { graphs },
        } = self;
        writeln!(f, "# Code Patterns (Grouped and represented in Lattices)")?;
        writeln!(f, "{STYLE}")?;

        // summary table
        writeln!(f, "### SubLattice stats")?;
        writeln!(f, "| graph | {} |", LatticeStats::header())?;
        writeln!(f, "|-------|-------|-------|-------|-------|-------|")?;
        for (i, (stats, _)) in graphs.iter().enumerate() {
            writeln!(f, "| {i} | {} |", stats)?;
        }

        // the good generalized patterns
        let summary_head = |f: &mut Fmt<'_>| writeln!(f, "Generalized patterns");
        let summary_content = |_f: &mut Fmt<'_>| Ok(());
        let details_content = |f: &mut Fmt<'_>| {
            for (i, (stats, graph)) in graphs.iter().enumerate() {
                use crate::lattice_graph::TopStats;

                if stats.complete_tops.is_empty() {
                    continue;
                }
                writeln!(f, "#### Sublattice {i}")?;
                for (node_id, stats) in TopStats::iter_covering_all(
                    (stats.complete_tops.iter())
                        .chain(stats.incompletes_tops.iter())
                        .map(|(node_id, stats)| (*node_id, stats)),
                    &mut stats.uniqs.clone(),
                ) {
                    let query = graph.node_weight(node_id).unwrap();
                    let pattern = lattice.pretty(query.borrow());
                    let incompletes = if stats.incomplete {
                        " (incomplete)"
                    } else {
                        ""
                    };
                    let query = query.pp();
                    writeln!(f, "<a href='#{query}'>{query}</a>{incompletes}")?;
                    writeln!(f, "```scheme\n{pattern}\n```")?;
                }
            }
            Ok(())
        };
        pp_detail_block(
            f,
            false,
            &"tops",
            summary_head,
            summary_content,
            details_content,
        )?;

        let summary_head = |f: &mut Fmt<'_>| writeln!(f, "Incomplete Generalized patterns");
        let summary_content = |_f: &mut Fmt<'_>| Ok(());
        let details_content = |f: &mut Fmt<'_>| {
            for (i, (stats, graph)) in graphs.iter().enumerate() {
                use crate::lattice_graph::TopStats;

                if !stats.complete_tops.is_empty() {
                    continue;
                }
                writeln!(f, "")?;
                writeln!(f, "#### Sublattice {i} (no complete generalized pattern)")?;
                for (node_id, stats) in TopStats::iter_covering_all(
                    stats
                        .incompletes_tops
                        .iter()
                        .map(|(node_id, stats)| (*node_id, stats)),
                    &mut stats.uniqs.clone(),
                ) {
                    let query = graph.node_weight(node_id).unwrap();
                    let pattern = lattice.pretty(&query.borrow());
                    let incompletes = if stats.incomplete {
                        " (incomplete)"
                    } else {
                        ""
                    };
                    let query = query.pp();
                    writeln!(f, "<a href='#{query}'>{query}</a>{incompletes}")?;
                    writeln!(f, "```scheme\n{pattern}\n```")?;
                }
            }
            Ok(())
        };
        pp_detail_block(
            f,
            false,
            &"incomplete-tops",
            summary_head,
            summary_content,
            details_content,
        )?;

        // content
        for (i, (stats, graph)) in graphs.iter().enumerate() {
            writeln!(f, "## Connex SubLattice {i}")?;
            writeln!(f, "| nodes | edges | leafs |")?;
            writeln!(f, "|-------|-------|-------|")?;
            writeln!(f, "| {} |", stats)?;
            writeln!(f, "")?;
            // let dot = petgraph::dot::Dot::new(&graph);
            // writeln!(file, "```graphviz\n{}\n```", dot)?;

            #[cfg(feature = "lattice")]
            print_mermaid_graph(f, lattice, stores, graph)?;
            print_patterns(f, lattice, stores, graph, stats)?;
        }
        Ok(())
    }
}
const EDGE_LIMIT: usize = 1000;

#[cfg(feature = "lattice")]
fn print_mermaid_graph<HAST: HyperAST, Q, P>(
    f: &mut std::fmt::Formatter<'_>,
    _lattice: &QueryLattice<P>,
    _stores: &HAST,
    graph: &petgraph::Graph<Q, enumset::EnumSet<crate::code2query::TrMarker>>,
) -> Result<(), std::fmt::Error>
where
    Q: Borrow<IdNQ> + PQ,
    for<'t> (&'t HAST, P): PPP,
{
    if graph.edge_count() > EDGE_LIMIT {
        writeln!(f, "> too many edges (over 1000)")?;
    } else {
        let mermaid = Mermaid::new(&graph);
        writeln!(f, "```mermaid\n{}\n```", mermaid)?;
    }
    Ok(())
}

fn print_patterns<HAST: HyperAST, Q, P>(
    f: &mut std::fmt::Formatter<'_>,
    lattice: &QueryLattice<P>,
    stores: &HAST,
    graph: &petgraph::Graph<Q, enumset::EnumSet<crate::code2query::TrMarker>>,
    stats: &crate::lattice_graph::LatticeStats,
) -> Result<(), std::fmt::Error>
where
    HAST::IdN: std::fmt::Debug + Copy,
    HAST::IdN: hyperast::types::NodeId<IdN = HAST::IdN>,
    for<'t> LendT<'t, HAST>: WithSerialization + WithStats,
    Q: Clone + std::borrow::Borrow<IdNQ> + PQ,
    P: hyperast::position::position_accessors::SolvedPosition<HAST::IdN> + Copy + Eq,
    for<'t> (&'t HAST, P): PPP,
{
    use crate::lattice_graph::pattern_stats;
    use petgraph::visit::EdgeRef;
    use petgraph::visit::IntoNodeReferences;
    use std::fmt::Formatter as Fmt;
    let patts: Vec<_> = graph
        .node_references()
        .map(|(id, query)| (id, pattern_stats(&lattice.query_store, *query.borrow())))
        .collect();

    let mut depths = vec![0; patts.len()];
    (0..patts.len()).rev().for_each(|i| {
        let mut inc = graph.edges_directed((i as u32).into(), petgraph::Direction::Incoming);
        if inc.next().is_some() {
            depths[i] = inc.map(|x| depths[x.target().index()]).min().unwrap_or(0) + 1;
        }
    });

    let mut ranked = (0..patts.len()).collect::<Vec<_>>();
    ranked.reverse();
    ranked.sort_by(|a, b| {
        std::cmp::Ordering::Equal
            .then(depths[*a].cmp(&depths[*b]))
            .then(stats.topo.inits(*a).cmp(&stats.topo.inits(*b)).reverse())
            .then(patts[*a].1.cmp(&patts[*b].1).reverse())
    });
    let mut opened = 0;

    for j in ranked.into_iter() {
        let (node_id, s) = &patts[j];
        if graph.edge_count() > EDGE_LIMIT {
            let mut inc = graph.edges_directed(*node_id, petgraph::Direction::Incoming);
            let mut out = graph.edges_directed(*node_id, petgraph::Direction::Outgoing);
            if !(inc.next().is_none() || out.next().is_none()) {
                continue;
            }
        }

        let open = opened < 4;
        if open {
            opened += 1;
        }
        let query = graph.node_weight(*node_id).unwrap();
        let id = query;
        let query = query.pp();
        let summary_head = |f: &mut Fmt<'_>| writeln!(f, "Pattern {query}");

        let query = *id.borrow();
        let summary_content = |f: &mut Fmt<'_>| {
            writeln!(f, "| leafs {}-----|", s.header())?;
            writeln!(f, "| {} | {s}", stats.topo.inits(j))?;
            writeln!(f, "")?;
            writeln!(f, "")?;
            let init_count = (lattice.raw_rels.get(&query).unwrap())
                .iter()
                .filter_map(|x| x.as_init())
                .count();

            if init_count == 0 {
                return Ok(());
            }

            let summary_head = |f: &mut Fmt<'_>| writeln!(f, "{init_count} inits");

            let summary_content = |_f: &mut Fmt<'_>| Ok(());

            let details_content = |f: &mut Fmt<'_>| pp_inits(lattice, stores, f, query);

            let id = id.pp();
            pp_detail_block(
                f,
                j < 3,
                &id,
                summary_head,
                summary_content,
                details_content,
            )
        };

        let pattern = lattice.pretty(&query);
        let details_content = |f: &mut std::fmt::Formatter<'_>| {
            let inc = graph.edges_directed(*node_id, petgraph::Direction::Incoming);
            let mut b = false;
            for inc in inc {
                if !b {
                    writeln!(f, "generalizes to ")?;
                }
                if b {
                    writeln!(f, ",")?;
                }
                let kind = inc.weight();
                let node_id = inc.source();
                let query = graph.node_weight(node_id).unwrap();
                let query = query.pp();
                write!(f, "<a href='#{query}'>{kind}:{query}</a>")?;
                b = true;
            }
            if b {
                writeln!(f, "")?;
                writeln!(f, "")?;
            }
            writeln!(f, "```scheme\n{}\n```", pattern)?;
            let mut b = false;
            for out in graph.edges(*node_id) {
                if !b {
                    writeln!(f, "specializes to ")?;
                }
                if b {
                    writeln!(f, ",")?;
                }
                let kind = out.weight();
                let node_id = out.target();
                let query = graph.node_weight(node_id).unwrap();
                let query = query.pp();
                write!(f, "<a href='#{query}'>{kind}:{query}</a>")?;
                b = true;
            }
            if b {
                writeln!(f, "")?;
            }

            Ok(())
        };
        let id = id.pp();
        pp_detail_block(f, open, &id, summary_head, summary_content, details_content)?;
    }
    Ok(())
}

pub(crate) fn pp_detail_block<W: std::fmt::Write>(
    f: &mut W, //f: &mut std::fmt::Formatter<'_>
    open: bool,
    id: &impl Display,
    summary_head: impl FnOnce(&mut W) -> std::fmt::Result,
    summary_content: impl FnOnce(&mut W) -> std::fmt::Result,
    details_content: impl FnOnce(&mut W) -> std::fmt::Result,
) -> std::fmt::Result {
    let open = if open { " open" } else { "" };
    writeln!(f, "<details{open} id=\"{id}\">")?;
    write!(f, "<summary>")?;
    summary_head(f).unwrap();
    writeln!(f, "")?;
    summary_content(f).unwrap();
    writeln!(f, "\n\n</summary>\n")?;
    details_content(f).unwrap();
    writeln!(f, "\n</details>\n")?;
    Ok(())
}

pub trait PPP {
    fn pp(&self) -> impl Display;
}
impl<HAST: HyperAST> PPP for (&HAST, &hyperast::store::nodes::legion::NodeIdentifier) {
    fn pp(&self) -> impl Display {
        self.1.pp()
    }
}
impl<HAST: HyperAST> PPP for (&HAST, hyperast::store::nodes::legion::NodeIdentifier) {
    fn pp(&self) -> impl Display {
        self.1.pp()
    }
}
impl<HAST: HyperAST> PPP for (&HAST, &StructuralPosition<HAST::IdN, HAST::Idx>)
where
    HAST::IdN: std::fmt::Debug + Copy,
    HAST::IdN: hyperast::types::NodeId<IdN = HAST::IdN>,
    for<'t> LendT<'t, HAST>: WithSerialization + WithStats,
{
    fn pp(&self) -> impl Display {
        type Pos<F, Idx> = hyperast::position::file_and_range::Position<F, Idx>;
        let p = PositionConverter::new(self.1)
            .with_stores(self.0)
            .compute_pos_post_order::<_, Pos<_, _>>();
        struct PP<Idx: PrimInt>(Pos<std::path::PathBuf, Idx>);
        impl<Idx: PrimInt> Display for PP<Idx> {
            fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
                write!(
                    f,
                    "{}:{:?}",
                    self.0.file().to_string_lossy(),
                    self.0.range(),
                )
            }
        }
        PP(p)
    }
}

pub(crate) fn pp_inits<HAST: HyperAST, P>(
    // lattice: &QueryLattice<&StructuralPosition<HAST::IdN, HAST::Idx>>,
    lattice: &QueryLattice<P>,
    stores: &HAST,
    file: &mut impl std::fmt::Write,
    query: IdNQ,
) -> std::fmt::Result
where
    HAST::IdN: std::fmt::Debug + Copy,
    HAST::IdN: hyperast::types::NodeId<IdN = HAST::IdN>,
    for<'t> LendT<'t, HAST>: WithSerialization + WithStats,
    P: hyperast::position::position_accessors::SolvedPosition<HAST::IdN> + Copy,
    for<'t> (&'t HAST, P): PPP,
{
    let inits = lattice
        .raw_rels
        .get(&query)
        .unwrap()
        .iter()
        .filter_map(|x| {
            let crate::code2query::TR::Init(x) = x else {
                return None;
            };
            Some(x)
        });
    for x in inits {
        // use hyperast::position::position_accessors::SolvedPosition as _;

        // type Pos<IdN, Idx> = hyperast::position::file_and_range::Position<IdN, Idx>;
        // let p = PositionConverter::new(*x)
        //     .with_stores(stores)
        //     .compute_pos_post_order::<_, Pos<_, _>>();
        // writeln!(file, "#### {}:{:?}", p.file().to_string_lossy(), p.range(),)?;
        writeln!(file, "#### {}", PPP::pp(&(stores, *x)))?;
        let txt = hyperast::nodes::TextSerializer::new(stores, x.node());
        writeln!(file, "```java\n{}\n```", txt)?;
    }
    Ok(())
}

pub trait PQ {
    fn pp(&self) -> impl Display;
}
impl PQ for IdNQ {
    fn pp(&self) -> impl Display {
        PrettyQ(*self)
    }
}
#[derive(Clone)]
pub struct PrettyQ(pub(crate) IdNQ);
impl std::fmt::Display for PrettyQ {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let id: u64 = unsafe { std::mem::transmute(self.0) };
        write!(f, "{:X}", id)
    }
}
impl From<IdNQ> for PrettyQ {
    fn from(value: IdNQ) -> Self {
        PrettyQ(value)
    }
}
impl std::ops::Deref for PrettyQ {
    type Target = IdNQ;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

#[cfg(feature = "lattice")]
mod mermaid {
    use std::fmt;
    use std::fmt::Display;

    use petgraph::dot::Config;
    use petgraph::visit::EdgeRef;
    use petgraph::visit::GraphProp;
    use petgraph::visit::NodeIndexable;
    use petgraph::visit::NodeRef;
    use petgraph::visit::{IntoEdgeReferences, IntoNodeReferences};

    macro_rules! make_config_struct {
        ($($variant:ident,)*) => {
            #[allow(non_snake_case)]
            #[derive(Default)]
            struct Configs {
                $($variant: bool,)*
            }
            impl Configs {
                #[inline]
                fn extract(configs: &[Config]) -> Self {
                    let mut conf = Self::default();
                    for c in configs {
                        match *c {
                            $(Config::$variant => conf.$variant = true,)*
                            Config::_Incomplete(()) => {}
                        }
                    }
                    conf
                }
            }
        }
    }
    make_config_struct!(
        NodeIndexLabel,
        EdgeIndexLabel,
        EdgeNoLabel,
        NodeNoLabel,
        GraphContentOnly,
    );

    pub struct Mermaid<'a, G>
    where
        G: IntoEdgeReferences + IntoNodeReferences,
    {
        graph: G,
        get_edge_attributes: &'a dyn Fn(G, G::EdgeRef) -> String,
        get_node_attributes: &'a dyn Fn(G, G::NodeRef) -> String,
        config: Configs,
    }

    impl<'a, G> Mermaid<'a, G>
    where
        G: IntoNodeReferences + IntoEdgeReferences,
    {
        /// Create a `Mermaid` formatting wrapper with default configuration.
        #[inline]
        pub fn new(graph: G) -> Self {
            Self::with_config(graph, &[])
        }

        /// Create a `Dot` formatting wrapper with custom configuration.
        #[inline]
        pub fn with_config(graph: G, config: &'a [Config]) -> Self {
            Self::with_attr_getters(graph, config, &|_, _| String::new(), &|_, _| String::new())
        }

        #[inline]
        pub fn with_attr_getters(
            graph: G,
            config: &'a [Config],
            get_edge_attributes: &'a dyn Fn(G, G::EdgeRef) -> String,
            get_node_attributes: &'a dyn Fn(G, G::NodeRef) -> String,
        ) -> Self {
            let config = Configs::extract(config);
            Self {
                graph,
                get_edge_attributes,
                get_node_attributes,
                config,
            }
        }
    }

    impl<G> fmt::Display for Mermaid<'_, G>
    where
        G: IntoEdgeReferences + IntoNodeReferences + NodeIndexable + GraphProp,
        G::EdgeWeight: fmt::Display,
        G::NodeWeight: super::PQ,
    {
        fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
            use crate::synth_display::PQ;
            // let n_fmt = fmt::Display::fmt;
            let n_fmt = |n: &G::NodeWeight, f: &mut fmt::Formatter| {
                write!(f, "<a href='#{}'>{}</a>", n.pp(), n.pp())
            };
            self.graph_fmt(f, n_fmt, fmt::Display::fmt)
        }
    }

    static TYPE: [&str; 2] = ["graph TD", "graph TD"];
    static EDGE: [&str; 2] = ["--", "-->"];
    static INDENT: &str = "    ";

    impl<G> Mermaid<'_, G>
    where
        G: IntoEdgeReferences + IntoNodeReferences + NodeIndexable + GraphProp,
        G::EdgeWeight: fmt::Display,
        // G::NodeWeight: super::PQ,
    {
        fn graph_fmt<NF, EF>(
            &self,
            f: &mut fmt::Formatter,
            node_fmt: NF,
            edge_fmt: EF,
        ) -> fmt::Result
        where
            NF: Fn(&G::NodeWeight, &mut fmt::Formatter) -> fmt::Result,
            EF: Fn(&G::EdgeWeight, &mut fmt::Formatter) -> fmt::Result,
        {
            let g = self.graph;
            if !self.config.GraphContentOnly {
                writeln!(f, "{}", TYPE[g.is_directed() as usize])?;
            }

            // output all labels
            for node in g.node_references() {
                write!(f, "{}{}[", INDENT, g.to_index(node.id()),)?;
                if !self.config.NodeNoLabel {
                    // write!(f, "label = \"")?;
                    if self.config.NodeIndexLabel {
                        write!(f, "{}", g.to_index(node.id()))?;
                    } else {
                        Escaped(FnFmt(node.weight(), &node_fmt)).fmt(f)?;
                    }
                    // write!(f, "\" ")?;
                }
                writeln!(f, "{}]", (self.get_node_attributes)(g, node))?;
            }
            // output all edges
            for (i, edge) in g.edge_references().enumerate() {
                write!(f, "{}{} {} ", INDENT, g.to_index(edge.source()), EDGE[0],)?;
                if !self.config.EdgeNoLabel {
                    // write!(f, "label = \"")?;
                    if self.config.EdgeIndexLabel {
                        write!(f, " {} ", i)?;
                    } else {
                        Escaped(FnFmt(edge.weight(), &edge_fmt)).fmt(f)?;
                        // FnFmt(edge.weight(), &edge_fmt).fmt(f)?;
                    }
                    // write!(f, "\" ")?;
                }
                write!(f, "{}", (self.get_edge_attributes)(g, edge))?;
                writeln!(
                    f,
                    " {} {}",
                    EDGE[g.is_directed() as usize],
                    g.to_index(edge.target()),
                )?;
            }

            if !self.config.GraphContentOnly {
                writeln!(f, "")?;
            }
            Ok(())
        }
    }

    /// Pass Display formatting through a simple escaping filter
    struct Escaped<T>(T);

    impl<T> fmt::Display for Escaped<T>
    where
        T: fmt::Display,
    {
        fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
            use std::fmt::Write;
            if f.alternate() {
                writeln!(&mut Escaper(f), "{:#}", &self.0)
            } else {
                write!(&mut Escaper(f), "{}", &self.0)
            }
        }
    }

    /// Format data using a specific format function
    struct FnFmt<'a, T, F>(&'a T, F);

    impl<'a, T, F> fmt::Display for FnFmt<'a, T, F>
    where
        F: Fn(&'a T, &mut fmt::Formatter<'_>) -> fmt::Result,
    {
        fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
            self.1(self.0, f)
        }
    }

    /// Escape for Graphviz
    struct Escaper<W>(W);

    impl<W> fmt::Write for Escaper<W>
    where
        W: fmt::Write,
    {
        fn write_str(&mut self, s: &str) -> fmt::Result {
            for c in s.chars() {
                self.write_char(c)?;
            }
            Ok(())
        }

        fn write_char(&mut self, c: char) -> fmt::Result {
            match c {
                '"' | '\\' => self.0.write_char('\\')?,
                // \l is for left justified linebreak
                '\n' => return self.0.write_str("\\l"),
                _ => {}
            }
            self.0.write_char(c)
        }
    }
}
