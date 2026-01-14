use num::ToPrimitive;
use petgraph::data::FromElements;
use petgraph::visit::NodeCount;
use serde::Serialize;

use hyperast::position::StructuralPosition;
use hyperast::types::WithStats;
use hyperast_gen_ts_tsquery::code2query::QueryLattice;
use hyperast_vcs_git::TStore;

use super::IdNQ;
use super::graph_compression;

use super::IdN;
type HAST = hyperast::store::SimpleStores<TStore>;

#[derive(Clone, Serialize)]
pub struct Payload<S, F>(pub S, pub F);

pub type G = G2;
pub type G1 = Vec<petgraph::Graph<Payload<usize, std::string::String>, ()>>;

// StructuralPosition<IdN, Idx>
pub fn prep1<P: PartialEq, F: Fn(&P) -> String>(lattice: &QueryLattice<&P>, f: F) -> G1 {
    // NOTE at this point is it not a lattice just a poset
    let graph = make_lattice_graph(
        lattice,
        |query| {
            let t = (lattice.query_store.node_store)
                .resolve(query)
                .size_no_spaces();
            let mut s = lattice.pretty(&query);
            let inits = (lattice.raw_rels.get(&query).unwrap().iter())
                .filter_map(|x| x.as_init().map(|x| f(*x)));
            s.push_str("\n-----------------------\n");
            s.extend(inits);

            Payload(t, s)
        },
        |_, _, _| (),
    );
    // Now it will be a lattice (multiple cc al top and bot are implicit)
    group_lattices(graph)
}

pub type G2 = Vec<_G>;
#[derive(Serialize)]
pub struct _G {
    /// hold the examples indexes
    // ex: Vec<u32>,
    /// hold the query ids
    pub queries: Vec<u32>,
    /// successors (ie. to more general patterns in our case)
    /// multiple opti based on topo sorted lattice:
    /// * sub lists are separated by zero
    /// * each succ is encoded as the difference with prev succ
    ///   * when first the index of the node is the one subtracted from succ
    succ: Vec<u32>,

    // tr_markers: Vec<TrMarkers>,
    /// not necessarily there
    pub queries_pretty: Vec<String>,
    /// pretty printed examples
    pub ex_pretty: Vec<String>,
    /// query associated to pretty printed examples
    ///
    /// same size as ex_pretty, sorted, and delta encoded similarly to succ,
    /// but here an example has exactly one associated query
    pub ex_query: Vec<u32>,
}
#[derive(Clone, Copy)]
enum TrKinds {
    /// remove stuff from a pattern, possibly making it invalid
    Remove,
    /// remove stuff that cannot make a pattern invalid,
    /// by themselves I think very few removal meta patterns are always robust,
    /// in the case of constraining by merging positional predicates,
    /// by ensuring no captures and capture-based predicates are removed
    /// some cases of robust ones are:
    /// `(predicate (identifier) (#EQ? "EQ")) @rm`,
    /// `(named_node (named_node (identifier) .) @rm .)`,
    /// `(named_node (field_definition (named_node (identifier) .)) @rm .)`,
    /// `(named_node (anonymous_node) @rm .)`,
    /// `(named_node (field_definition (anonymous_node)) @rm .)`,
    /// `(named_node (named_node (identifier) .) @rm . [(named_node) (anonymous_node) (field_definition)])`,
    /// `(named_node (anonymous_node) @rm . [(named_node) (anonymous_node) (field_definition)])`.
    ///
    /// NOTE adding constraints to meta patterns should not make them less robust
    /// NOTE if you can make sure you never add a capture or a positional predicate to a node you might remove, then the removal is robust.
    /// TODO most of the robust removal previously defined could be simplified if there was a specific syntax for positional predicates putting them together with captures
    /// NOTE this new syntax is not necessarily one defined in tree-sitter, but most likely in tree-sitter-query, considering Upper Camel Case as positional predicates.
    RobustRemove,
    /// adds a constraint between things,
    /// e.g. captures considered equal `(#eq p0 p1)`,
    /// afterward a removal might remove one of the component making it partial thus invalid
    Constrain,
}
#[derive(Clone, Copy)]
/// represents the constrained status of a generalization path
enum ConstrainedStatus {
    Unconstrained,
    Constrained,
    RemovalOnConstrained,
}
// see [`ConstrainedStatus::generalize`]
impl std::ops::AddAssign<TrKinds> for ConstrainedStatus {
    fn add_assign(&mut self, rhs: TrKinds) {
        self.generalize(rhs);
    }
}
impl std::ops::BitOr for ConstrainedStatus {
    type Output = Self;

    fn bitor(self, rhs: Self) -> Self::Output {
        self.merge_generalization_paths(rhs)
    }
}
pub trait Constraining {
    type Tr;
    fn generalize(&mut self, other: Self::Tr);
    fn merge_generalization_paths(self, other: Self) -> Self;
}

impl Constraining for () {
    type Tr = ();
    fn generalize(&mut self, other: Self::Tr) {}
    fn merge_generalization_paths(self, other: Self) -> Self {
        
    }
}

impl Constraining for ConstrainedStatus {
    type Tr = TrKinds;
    /// models what happens when appending a generalization to a generalization path
    fn generalize(&mut self, other: TrKinds) {
        use TrKinds::*;
        *self = match *self {
            // e.g., doing an EQ generalization, constrain the created pattern
            Self::Unconstrained if matches!(other, Constrain) => Self::Constrained,
            // removing something in a constrained pattern might produce an invalid pattern
            Self::Constrained if matches!(other, Remove) => Self::RemovalOnConstrained,
            // [`TrKinds::RobustRemove`] does not change the state
            // in all other cases the state does not change either
            x => x,
        };
    }
    /// models what happens when two generalization paths have the same generalized pattern,
    /// thus we take the most valid status
    fn merge_generalization_paths(self, other: ConstrainedStatus) -> ConstrainedStatus {
        match self {
            Self::Unconstrained => Self::Unconstrained,
            Self::Constrained => Self::Constrained,
            Self::RemovalOnConstrained => other,
        }
    }
}

enum ValidityStatus {
    Valid,
    PossiblyInvalid,
}

impl From<ConstrainedStatus> for ValidityStatus {
    fn from(status: ConstrainedStatus) -> Self {
        match status {
            ConstrainedStatus::Unconstrained => ValidityStatus::Valid,
            ConstrainedStatus::Constrained => ValidityStatus::Valid,
            ConstrainedStatus::RemovalOnConstrained => ValidityStatus::PossiblyInvalid,
        }
    }
}

use hyperast_gen_ts_tsquery::code2query::TrMarker;
use hyperast_gen_ts_tsquery::code2query::TrMarkers;

impl _G {
    pub fn succs(&self) -> impl Iterator<Item = (u32, &[u32])> {
        self.succ
            .split(|x| *x == 0)
            .filter(|_| !self.succ.is_empty())
            .enumerate()
            // .filter(|_| !self.ex_query.is_empty() && !self.queries.is_empty())
            .map(|(i, succ)| (i as u32, succ))
    }
    pub fn tops(&self) -> impl Iterator<Item = IdNQ> {
        self.succs().filter(|&(x, succ)| succ.is_empty()).map(|(x, succ)| u64_to_idn(self.queries[x as usize] as u64))
    }
}

pub fn prep2<P: PartialEq + Clone + Into<IdN>, F: Fn(&IdN) -> String>(
    lattice: &QueryLattice<&P>,
    f: F,
) -> G2 {
    let g = Prep::extract_and_group(lattice);
    g.describe();
    g.log(&f);
    g.compress(&f)
}

pub struct Prep<'a, P> {
    g: Vec<petgraph::Graph<IdNQ, ()>>,
    lattice: &'a QueryLattice<&'a P>,
}

impl<'a, P> Prep<'a, P> {
    pub fn extract_and_group(lattice: &'a QueryLattice<&'a P>) -> Self
    where
        P: PartialEq + Clone + Into<IdN>,
    {
        dbg!();
        for (i, q) in lattice.queries.iter().enumerate() {
            print!("{i} {} ", q.1.len());
            println!(
                "{:?}",
                (lattice.raw_rels.get(&q.0).unwrap().iter())
                    .filter_map(|x| x.no_init())
                    .map(|x| lattice.queries.iter().position(|y| y.0 == *x).unwrap())
                    .collect::<Vec<_>>()
            );
        }
        // dbg!(lattice.queries.iter().map(|x| &x.1).collect::<Vec<_>>());
        // NOTE at this point is it not a lattice just a poset
        let graph = make_lattice_graph(
            lattice,
            |query| {
                query
                // let t = (lattice.query_store.node_store)
                //     .resolve(query)
                //     .size_no_spaces();
                // let mut s = lattice.pretty(&query);
                // let inits = (lattice.raw_rels.get(&query).unwrap().iter())
                //     .filter_map(|x| x.as_init().copied()).collect();
                // inits
            },
            |_, _, _| (),
        );
        // Now it will be a lattice (multiple cc as top is implicit)
        let g = group_lattices(graph);
        Self { g, lattice }
    }

    pub fn describe(&self)
    where
        P: PartialEq + Clone + Into<IdN>,
    {
        for (i, g) in self.g.iter().enumerate() {
            describe_lattice(g, self.lattice, i);
        }
    }

    pub fn log<F: Fn(&IdN) -> String>(&self, f: &F)
    where
        P: PartialEq + Clone + Into<IdN>,
    {
        #[cfg(feature = "rerun")]
        if let Some(rec) = rerun::RecordingStream::global(rerun::StoreKind::Recording) {
            if let Err(err) = log_lattices(&rec, &self.g, self.lattice, &f) {
                log::error!("Failed to log lattice: {}", err);
            }
        }
    }

    pub fn compress<F: Fn(&IdN) -> String>(self, f: &F) -> G2
    where
        P: PartialEq + Clone + Into<IdN>,
    {
        compress(self.g, &|q| self.lattice.pretty(q), &|q: &IdNQ| {
            self.lattice.raw_rels[q]
                .iter()
                .filter_map(|x| x.as_init())
                .map(|x| f(&(*x).clone().into()))
                .collect()
        })
        // compress_webgraphs(g)
    }

    pub(crate) fn tops(&self) -> impl Iterator<Item = (String, Vec<u32>)> {
        use petgraph::Direction::Incoming as Up;
        use petgraph::Direction::Outgoing as Down;
        fn tops(
            g: &petgraph::Graph<IdNQ, ()>,
        ) -> impl Iterator<Item = petgraph::prelude::NodeIndex> {
            g.node_indices()
                .filter(|x| g.neighbors_directed(*x, Up).count() == 0)
        }
        fn tops_plus_one(
            g: &petgraph::Graph<IdNQ, ()>,
        ) -> impl Iterator<Item = petgraph::prelude::NodeIndex> {
            tops(g).flat_map(|x| g.neighbors_directed(x, Down).chain([x]))
        }
        self.g
            .iter()
            .flat_map(move |g| {
                let mut dedup = hashbrown::HashSet::new();
                tops_plus_one(g)
                    .map(|x| g[x])
                    .filter(move |x| dedup.insert(*x))
            })
            .map(|top| (self.lattice.pretty(&top), self.lattice.extract2(top).1))
    }
}

fn idn_to_u64(x: IdN) -> u64 {
    unsafe { std::mem::transmute::<_, _>(x) }
}
fn u64_to_idn(x: u64) -> IdN {
    unsafe { std::mem::transmute::<_, _>(x) }
}

fn idn_to_u32(x: IdN) -> Result<u32, u64> {
    let x = idn_to_u64(x);
    ToPrimitive::to_u32(&x).ok_or(x)
}

fn compress<F: Fn(&IdNQ) -> String>(
    g: Vec<petgraph::Graph<IdNQ, ()>>,
    pretty_q: &F,
    pretty_ex: &impl Fn(&IdNQ) -> Vec<String>,
) -> Vec<_G> {
    g.into_iter()
        .map(|g| _compress(g, pretty_q, pretty_ex))
        .collect()
}

fn _compress<F: Fn(&IdNQ) -> String>(
    mut g: petgraph::Graph<IdNQ, ()>,
    pretty_q: &F,
    pretty_ex: &impl Fn(&IdNQ) -> Vec<String>,
) -> _G {
    use num::ToPrimitive;
    let queries = (g.raw_nodes().iter())
        .map(|x| idn_to_u32(x.weight).expect("too many query subtrees to fit in u32"))
        .collect();
    let queries_pretty = (g.raw_nodes().iter())
        .map(|x| pretty_q(&x.weight))
        .collect();
    let mut succ = vec![];
    for (i, x) in g.node_indices().enumerate() {
        if !succ.is_empty() {
            succ.push(0);
        }
        let mut n = g
            .neighbors_directed(x, petgraph::Direction::Incoming)
            .map(|x| (x.index() as u32))
            .collect::<Vec<_>>();
        n.reverse(); // apparently inverted ...
        let mut i = i as u32;
        for x in n.into_iter() {
            succ.push(x.checked_sub(i).expect("a topologically sorted graph"));
            i = x;
        }
    }
    let (ex_pretty, mut ex_query) = (g.raw_nodes().iter().enumerate())
        .flat_map(|(i, x)| pretty_ex(&x.weight).into_iter().map(move |x| (x, i as u32)))
        .unzip::<_, _, _, Vec<_>>();
    // now delta encoding
    let mut x = 0u32;
    for y in ex_query.iter_mut() {
        let r = *y - x;
        x = *y;
        *y = r;
    }
    let r = _G {
        queries,
        queries_pretty,
        succ,
        ex_pretty,
        ex_query,
    };
    if cfg!(debug_assertions) {
        // check if it is bijective
        let _g = _uncompress(&r);
        assert_eq!(g.node_count(), _g.node_count());
        assert_eq!(g.edge_count(), _g.edge_count());
        for (x, _x) in g.node_indices().zip(_g.node_indices()) {
            assert_eq!(
                g.neighbors_directed(x, petgraph::Direction::Incoming)
                    .count(),
                _g.neighbors_directed(_x, petgraph::Direction::Incoming)
                    .count()
            );
            g.neighbors_directed(x, petgraph::Direction::Incoming)
                .zip(_g.neighbors_directed(_x, petgraph::Direction::Incoming))
                .for_each(|(x, _x)| assert_eq!(x, _x));
        }
    }
    r

    // let mut edges = (g.raw_edges().into_iter())
    //     .map(|x| {
    //         (
    //             x.source().index().to_u32().unwrap(),
    //             x.target().index().to_u32().unwrap(),
    //         )
    //     })
    //     .collect::<Vec<_>>();
    // edges.sort_by(|a, b| a.0.cmp(&b.0).then_with(|| a.1.cmp(&b.1)));
    // let (sources, targets) = edges.into_iter().unzip::<u32, u32, Vec<_>, Vec<_>>();
    // let sources = delta_encode(sources);
    // let targets = delta_encode(targets);
    // _G {
    //     // ex: vec![],
    //     queries,
    //     sources,
    //     targets,
    // }
}

fn _uncompress(mut g: &_G) -> petgraph::Graph<IdN, ()> {
    use num::Zero as _;
    let mut r = petgraph::Graph::<IdN, ()>::with_capacity(
        g.queries.len(),
        g.succ.iter().filter(|x| x.is_zero()).count() + 1,
    );
    for n in &g.queries {
        let w: IdN = unsafe { std::mem::transmute(*n as u64) };
        r.add_node(w);
    }
    for (x, succ) in g.succ.split(|x| x.is_zero()).enumerate() {
        let mut i = x as u32;
        for &succ in succ {
            i += succ;
            r.add_edge(i.into(), (x as u32).into(), ());
        }
    }
    r
}

fn delta_encode(sources: Vec<u32>) -> Vec<i32> {
    let mut s = 0;
    let sources = sources
        .into_iter()
        .map(|x| {
            let r = x as i32 - s as i32;
            if x < s {
                print!("{} ", s);
            }
            s = x;
            r
        })
        .collect();
    println!();
    dbg!(s);
    sources
}

fn describe_lattice<P: PartialEq>(
    g: &petgraph::Graph<IdN, ()>,
    lattice: &QueryLattice<&P>,
    i: usize,
) {
    println!("lattice {i} description:");
    for i in g.node_indices() {
        print!("{} {:?}", i.index(), idn_to_u64(g[i]));
        print!(
            " {:?}",
            (lattice.raw_rels.get(&g[i]).unwrap().iter())
                .filter_map(|x| x.as_init())
                .map(|_| 1)
                .collect::<Vec<_>>()
        );
        print!(
            "{:?}",
            (lattice.raw_rels.get(&g[i]).unwrap().iter())
                .filter_map(|x| x.no_init().map(|x| idn_to_u64(*x)))
                .collect::<Vec<_>>()
        );
        println!(
            "{:?}",
            g.neighbors_directed(i, petgraph::Direction::Outgoing)
                .map(|j| idn_to_u64(*g.node_weight(j).unwrap()))
                .collect::<Vec<_>>()
        );
    }
    if let Ok(numbers) = compute_topo_numbers::<P, u64>(g, lattice, 64) {
        print_topo_numbers(g, lattice, numbers, |x| num::PrimInt::count_ones(*x));
    } else if let Ok(numbers) = compute_topo_numbers::<P, u128>(g, lattice, 128) {
        print_topo_numbers(g, lattice, numbers, |x| num::PrimInt::count_ones(*x));
    } else if let Ok(numbers) = compute_topo_numbers::<P, num::BigUint>(g, lattice, 1024) {
        print_topo_numbers(g, lattice, numbers, |x| num::BigUint::count_ones(x) as u32);
    } else {
        for n in g.node_indices() {
            let x = g.neighbors_directed(n, petgraph::Direction::Outgoing);
            print!("{:2} ", x.count()); // direct targeted
        }
        println!();
        for n in g.node_indices() {
            let x = g.neighbors_directed(n, petgraph::Direction::Incoming);
            print!("{:2} ", x.count()); // direct targeting
        }
        println!();
        log::warn!("too many inits for other metrics")
    }
    // #[cfg(feature = "rerun")]
    // if let Some(rec) = rerun::RecordingStream::global(rerun::StoreKind::Recording) {
    //     if let Err(err) = log_lattice(&rec, g, lattice, i) {
    //         log::error!("Failed to log lattice: {}", err);
    //     }
    // }
}

#[cfg(feature = "rerun")]
fn log_lattice<P: PartialEq>(
    rec: &rerun::RecordingStream,
    g: &petgraph::Graph<IdN, ()>,
    lattice: &QueryLattice<&P>,
    i: usize,
) -> Result<(), rerun::RecordingStreamError> {
    use rerun::external::arrow::array::ArrowNativeTypeOp;
    let path = format!("lattice/{i}");
    let node_ids = g.node_indices().map(|x| format!("{}", x.index()));
    let pretty = g
        .node_indices()
        .map(|x| format!("{}", lattice.pretty(&g[x])));
    let graph_nodes = rerun::GraphNodes::new(node_ids.clone().chain(node_ids.clone()))
        .with_labels(node_ids.chain(pretty))
        .with_many_show_labels((0..g.node_count()).map(|_| true).chain((0..g.node_count()).map(|i| i==0)))
        .columns([g.node_count(), g.node_count()]).unwrap()
        // .with_colors(colors)
    ;
    rec.send_columns(path.as_str(), [], graph_nodes)?;

    let graph_edges = rerun::GraphEdges::new(g.edge_references().map(|x| {
        use petgraph::visit::EdgeRef;
        (
            format!("{}", x.target().index()),
            format!("{}", x.source().index()),
        )
    }))
    .with_directed_edges();
    rec.log(path.as_str(), &graph_edges)
}

#[cfg(feature = "rerun")]
// a bit ugly but it does the job
fn log_lattices<P: PartialEq + Clone + Into<IdN>, F: Fn(&IdN) -> String>(
    rec: &rerun::RecordingStream,
    g: &Vec<petgraph::Graph<IdN, ()>>,
    lattice: &QueryLattice<&P>,
    f: &F,
) -> Result<(), rerun::RecordingStreamError> {
    let counted_as_vec = |count, i| (0..count).into_iter().map(|x| (i, x)).collect::<Vec<_>>();
    let path = format!("lattice");
    let inits_it = (lattice.raw_rels.iter())
        .flat_map(|x| {
            x.1.iter()
                .enumerate()
                .map(move |(j, tr)| (idn_to_u32(*x.0).unwrap(), j, tr))
        })
        .filter_map(|(i, j, tr)| tr.as_init().map(|x| (i, j, *x)));
    let inits_ids = inits_it.clone().map(|(i, j, x)| format!("{i}_{j}"));
    let inits_labels = inits_it.map(|(i, j, x)| f(&x.clone().into()));
    let node_ids = (g.iter().enumerate())
        .flat_map(move |(i, g)| counted_as_vec(g.node_count(), i))
        .map(|(i, x)| format!("{i}_{}", x));
    let pretty = (g.iter().enumerate())
        .flat_map(move |(i, g)| counted_as_vec(g.node_count(), i))
        .map(move |(i, x)| {
            let x = petgraph::graph::NodeIndex::new(x);
            format!("{}", lattice.pretty(&g[i][x]))
        });
    let graph_nodes = rerun::GraphNodes::new(node_ids.clone().chain(node_ids.clone()).chain(inits_ids))
        .with_labels(node_ids.chain(pretty).chain(inits_labels))
        // .with_colors(colors)
    ;
    rec.log(path.as_str(), &graph_nodes)?;

    let edges = (g.iter().enumerate())
        .flat_map(move |(i, g)| counted_as_vec(g.edge_count(), i))
        .map(|(i, x)| {
            use petgraph::visit::EdgeRef;
            let (source, target) = g[i].edge_endpoints((x as u32).into()).unwrap();
            (
                format!("{i}_{}", target.index()),
                format!("{i}_{}", source.index()),
            )
        });

    let inits_edges = (g.iter().enumerate())
        .flat_map(move |(i, g)| counted_as_vec(g.node_count(), i))
        .flat_map(|(i, x)| {
            let p = g[i][petgraph::graph::NodeIndex::new(x)];
            (lattice.raw_rels.get(&p).unwrap())
                .iter()
                .enumerate()
                .filter_map(move |(j, tr)| tr.as_init().map(|_| (i, j, x, p)))
        })
        .map(|(i, j, x, p)| (format!("{}_{j}", idn_to_u64(p)), format!("{i}_{x}")));
    let graph_edges = rerun::GraphEdges::new(edges.chain(inits_edges)).with_directed_edges();
    rec.log(path.as_str(), &graph_edges)
}

fn print_topo_numbers<P, Bitset>(
    g: &petgraph::Graph<IdN, ()>,
    lattice: &QueryLattice<&P>,
    numbers: TopoNumbers<Bitset>,
    count_ones: impl Fn(&Bitset) -> u32,
) where
    P: PartialEq,
    Bitset: num::One + num::Zero + Clone,
    Bitset: Eq,
    Bitset: std::ops::Shl<usize, Output = Bitset>,
    Bitset: std::ops::BitAnd<Bitset, Output = Bitset>,
{
    println!("(#child, #parent, inits, total inits, total patts)");
    for n in g.node_indices() {
        let x = g.neighbors_directed(n, petgraph::Direction::Incoming);
        print!("{:2} ", x.count()); // direct targeted
        let x = g.neighbors_directed(n, petgraph::Direction::Incoming);
        print!("{:2?} ", x.map(|x| x.index()).collect::<Vec<_>>()); // direct targeted
    }
    println!();
    for n in g.node_indices() {
        let x = g.neighbors_directed(n, petgraph::Direction::Outgoing);
        print!("{:2} ", x.count()); // direct targeting
    }
    println!();
    let TopoNumbers {
        dense_inits_map,
        dense_inits_counts,
        reachable_inits: bitsets,
    } = numbers;
    for i in &dense_inits_map {
        print!("{:2} ", dense_inits_counts[*i as usize]);
    }
    println!();
    for i in bitsets.iter() {
        print!("{:2} ", count_ones(i) - 1);
    }
    println!();
    for i in bitsets.iter() {
        let c = total_inits(&dense_inits_counts, i);
        print!("{:2} ", c);
    }
    println!();
    let ex: usize = dense_inits_counts.iter().sum();
    println!("top level patterns:");
    for n in g.node_indices() {
        let x = g.neighbors_directed(n, petgraph::Direction::Incoming);
        if x.count() > 0 {
            continue;
        }
        let qid = g[n];
        let c = total_inits(&dense_inits_counts, &bitsets[n.index()]);
        let s = lattice.query_store.node_store.resolve(qid).size();
        if c == 0 {
            println!("---top--------inits:{c}/{ex}-----size:{s}--------\nERROR 0 inits",);
            continue;
        }
        println!(
            "---top--------inits:{c}/{ex}-----size:{s}--------\n{}",
            lattice.pretty(&qid).chars().take(200).collect::<String>(),
        );
    }
    println!("snd level patterns:");
    let mut snd_lvl = vec![];
    for n in g.node_indices() {
        let x = g.neighbors_directed(n, petgraph::Direction::Incoming);
        if x.count() > 0 {
            continue;
        }
        let c = total_inits(&dense_inits_counts, &bitsets[n.index()]);
        if c <= 1 {
            continue;
        }
        for n in g.neighbors_directed(n, petgraph::Direction::Outgoing) {
            let qid = g[n];
            let c = total_inits(&dense_inits_counts, &bitsets[n.index()]);
            let s = lattice.query_store.node_store.resolve(qid).size();
            snd_lvl.push((qid, c, s));
        }
    }
    snd_lvl.sort_by_key(|x| (x.1, x.2));
    let inits = dense_inits_counts.len();
    for (qid, c, s) in snd_lvl {
        println!("---snd-----inits:{c}/{ex}-----size:{s}------",);
        if c > 1 {
            println!("{}", lattice.pretty(&qid),);
        }
    }
}

fn total_inits<Bitset>(dense_inits_counts: &Vec<usize>, i: &Bitset) -> usize
where
    Bitset: Eq + Clone,
    Bitset: num::One + num::Zero,
    Bitset: std::ops::Shl<usize, Output = Bitset>,
    Bitset: std::ops::BitAnd<Bitset, Output = Bitset>,
{
    // total inits for this pattern
    let mut c = 0;
    // iter each bit
    for j in 0..dense_inits_counts.len() {
        if (i.clone() & (Bitset::one() << j)) != Bitset::zero() {
            c += dense_inits_counts[j];
        }
    }
    c
}

struct TopoNumbers<Bitset> {
    // NOTE always sequential iteration (for now at least)
    // TODO try to use some compression, like EF ?
    dense_inits_map: Vec<Imap>,
    dense_inits_counts: Vec<usize>,
    reachable_inits: ReachableInits<Bitset>,
}
type Imap = u8;

impl<Bitset> TopoNumbers<Bitset>
where
    Bitset: num::One + Clone,
    Bitset: std::ops::Shl<usize, Output = Bitset>,
    Bitset: std::ops::BitOrAssign + std::ops::BitOrAssign,
{
    fn compute<P>(
        g: &petgraph::Graph<IdN, ()>,
        lattice: &QueryLattice<&P>,
        max: u32,
    ) -> Result<TopoNumbers<Bitset>, PreparedInits>
    where
        P: PartialEq,
    {
        for i in 0..g.node_count() {
            let n = petgraph::prelude::NodeIndex::new(i);
            eprint!("|{:>3} {} ", i, idn_to_u64(*g.node_weight(n).unwrap()));
            for j in g.neighbors_directed(n, petgraph::Direction::Outgoing) {
                eprint!("{}:{},", j.index(), idn_to_u64(*g.node_weight(j).unwrap()));
            }
            eprintln!();
        }
        let csr = graph_compression::graph_to_csr(g);
        assert_eq!(g.node_count(), csr.node_count());
        assert_eq!(g.edge_count(), csr.edge_count());
        // for n in 0..csr.node_count() {
        //     use petgraph::visit::IntoNeighbors;
        //     let x = csr.neighbors(n as u32); //, petgraph::Direction::Outgoing
        //     print!("{:2} ", x.count()); // direct targeted
        //     let x = csr.neighbors(n as u32);
        //     print!("{:2?} ", x.collect::<Vec<_>>()); // direct targeted
        // }
        // println!();
        let preped = PreparedInits::compute(&csr, |query| {
            (lattice.raw_rels.get(&query).unwrap())
                .iter()
                .filter_map(|x| x.as_init().copied())
                .count()
        });
        if preped.dense_inits_counts.len() >= max as usize {
            return Err(preped);
        }
        let mut reachable_inits = ReachableInits::new(csr.node_count());
        let mut min_dist_inits = MinDistInits::<u8>::new(csr.node_count());
        let mut max_dist_inits = MaxDistInits::<u8>::new(csr.node_count());
        // indexes into dense_inits_counts
        for (i, _e) in iter_topo(&csr) {
            eprint!("{} {}", i, idn_to_u64(_e));
            // retrieve
            let curr = reachable_inits.retrieve(&preped, i);
            let curr_min = min_dist_inits.retrieve(&preped, i);
            let curr_max = max_dist_inits.retrieve(&preped, i);
            eprint!("({}) ", curr_min);
            // then diffuse
            for j in iter_succ(&csr, i) {
                eprint!("{},", j);

                reachable_inits.diffuse(j, &curr);
                min_dist_inits.diffuse(j, &curr_min);
                max_dist_inits.diffuse(j, &curr_max);
            }
            eprintln!();
        }
        println!("min dist to an initial pattern:");
        for i in min_dist_inits.iter() {
            print!("{:2} ", i);
        }
        println!();
        println!("max dist to an initial pattern:");
        for i in max_dist_inits.iter() {
            print!("{:2} ", i);
        }
        println!();
        Ok(TopoNumbers {
            reachable_inits,
            dense_inits_map: preped.dense_inits_map,
            dense_inits_counts: preped.dense_inits_counts,
        })
    }
}

struct PreparedInits {
    dense_inits_map: Vec<Imap>,
    dense_inits_counts: Vec<usize>,
}
impl PreparedInits {
    fn compute(csr: &petgraph::csr::Csr<IdN, ()>, inits: impl Fn(IdN) -> usize) -> Self {
        let mut dense_inits_map = Vec::<Imap>::new();
        let mut dense_inits_counts = vec![0];
        // first is for no inits
        for (_, query) in iter_topo(csr) {
            let inits = inits(query);
            if inits > 0 {
                let len = dense_inits_counts.len();
                dense_inits_map.push(len as Imap);
                dense_inits_counts.push(inits);
            } else {
                dense_inits_map.push(0);
            }
        }
        Self {
            dense_inits_map,
            dense_inits_counts,
        }
    }
}

type IdG = usize;
fn iter_topo(csr: &petgraph::csr::Csr<IdN, ()>) -> impl Iterator<Item = (IdG, IdN)> {
    (0..csr.node_count().to_u32().expect("use a larger Ix")).map(|i| (i as usize, csr[i]))
}
fn iter_succ(csr: &petgraph::csr::Csr<IdN, ()>, i: IdG) -> impl Iterator<Item = IdG> {
    use petgraph::visit::IntoNeighbors;
    csr.neighbors(i as u32).map(|i| i as usize)
}

struct ReachableInits<Bitset>(Vec<Bitset>);

impl<Bitset> std::ops::Deref for ReachableInits<Bitset> {
    type Target = Vec<Bitset>;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl<Bitset> ReachableInits<Bitset>
where
    Bitset: num::One + Clone,
    Bitset: std::ops::Shl<usize, Output = Bitset>,
    Bitset: std::ops::BitOrAssign + std::ops::BitOrAssign,
{
    fn new(node_count: usize) -> Self {
        Self(vec![Bitset::one(); node_count])
    }
    fn retrieve(&mut self, preped: &PreparedInits, i: IdG) -> Bitset {
        let map = preped.dense_inits_map[i] as usize;
        self.0[i] |= Bitset::one() << map;
        self.0[i].clone()
    }
    fn diffuse(&mut self, j: IdG, curr: &Bitset) {
        self.0[j] |= curr.clone();
    }
}

struct MinDistInits<D>(Vec<D>);

impl<D> std::ops::Deref for MinDistInits<D> {
    type Target = Vec<D>;
    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl<D: hyperast::PrimInt> MinDistInits<D> {
    fn new(node_count: usize) -> Self {
        Self(vec![D::max_value(); node_count])
    }
    fn retrieve(&mut self, preped: &PreparedInits, i: IdG) -> D {
        let map = preped.dense_inits_map[i] as usize;
        if map > 0 {
            self.0[i] = D::zero();
        }
        self.0[i]
    }
    fn diffuse(&mut self, j: IdG, curr: &D) {
        self.0[j] = self.0[j].min(curr.saturating_add(D::one()));
    }
}

struct MaxDistInits<D>(Vec<D>);
impl<D> std::ops::Deref for MaxDistInits<D> {
    type Target = Vec<D>;
    fn deref(&self) -> &Self::Target {
        &self.0
    }
}
impl<D: hyperast::PrimInt> MaxDistInits<D> {
    fn new(node_count: usize) -> Self {
        Self(vec![D::zero(); node_count])
    }
    fn retrieve(&mut self, preped: &PreparedInits, i: IdG) -> D {
        self.0[i]
    }
    fn diffuse(&mut self, j: IdG, curr: &D) {
        self.0[j] = self.0[j].max(*curr + D::one());
    }
}

fn compute_topo_numbers<P, Bitset>(
    g: &petgraph::Graph<IdN, ()>,
    lattice: &QueryLattice<&P>,
    max: u32,
) -> Result<TopoNumbers<Bitset>, usize>
where
    P: PartialEq,
    Bitset: num::One + Clone,
    Bitset: std::ops::Shl<usize, Output = Bitset>,
    Bitset: std::ops::BitOrAssign + std::ops::BitOrAssign,
{
    TopoNumbers::compute(g, lattice, max).map_err(|x| x.dense_inits_counts.len())
}

pub fn make_lattice_graph<N, E: 'static, P: PartialEq>(
    lattice: &QueryLattice<&P>,
    f: impl Fn(IdN) -> N,
    g: impl Fn(petgraph::graph::NodeIndex, petgraph::graph::NodeIndex, SimpKind) -> E,
) -> petgraph::Graph<N, E> {
    use petgraph::prelude::*;
    use std::collections::HashMap;
    let mut graph: DiGraph<_, _> = Default::default();

    let mut q_map = HashMap::<IdN, NodeIndex>::with_capacity(lattice.queries.len());
    for i in 0..lattice.queries.len() {
        let k = lattice.queries[i].0;
        let t = f(k);
        let index = graph.add_node(t);
        let _ = q_map.insert(k, index);
    }
    let mut graph = graph;

    (lattice.raw_rels.iter())
        .filter_map(|(k, v)| q_map.get(k).map(|source| (*source, v)))
        .flat_map(|(source, v)| v.iter().map(move |v| (source, v)))
        .for_each(|(source, v)| {
            each(
                v,
                |_, _| (),
                |kind, target| {
                    if let Some(&target) = q_map.get(target) {
                        let e = g(target, source, kind);
                        graph.add_edge(target, source, e);
                    }
                },
            );
        });
    graph
}

pub fn group_lattices<N: Clone, E: Clone + 'static>(
    graph: petgraph::Graph<N, E>,
) -> Vec<petgraph::Graph<N, E, petgraph::Directed, petgraph::csr::DefaultIx>>
where
    E: Constraining,
{
    use petgraph::prelude::*;

    use petgraph::visit::NodeIndexable;
    let mut vertex_sets = petgraph::unionfind::UnionFind::new(graph.node_bound());
    for edge in graph.edge_references() {
        let (a, b) = (edge.source(), edge.target());

        // union the two vertices of the edge
        vertex_sets.union(graph.to_index(a), graph.to_index(b));
    }
    let labels = vertex_sets.into_labeling();
    let mut sorting: Vec<_> = (0..labels.len()).collect();
    sorting.sort_unstable_by_key(|i| labels[*i]);
    sorting.dedup_by_key(|i| labels[*i]);

    let scc = sorting
        .into_iter()
        .map(|i| labels[i])
        .map(|i| {
            (labels.iter().enumerate())
                .filter_map(|(l, j)| (i == *j).then_some(NodeIndex::new(l)))
                .collect::<Vec<_>>()
        })
        .collect::<Vec<_>>();

    dbg!(scc.len());

    let uninit = NodeIndex::new(UNINIT_INDEX);
    let mut map: Vec<NodeIndex> = vec![uninit; graph.node_count()];

    let mut singles = Graph::new();
    let mut graphs: Vec<_> = scc
        .into_iter()
        .filter_map(|connected_nodes| {
            if connected_nodes.len() == 1 {
                singles.add_node(graph.node_weight(connected_nodes[0]).unwrap().clone());
                return None;
            }
            let graph = from_cc(&graph, &mut map, connected_nodes);
            println!("cc:");
            for n in graph.node_indices() {
                let x = graph.neighbors_directed(n, petgraph::Direction::Incoming);
                print!("{:2} ", x.count()); // direct targeted
                let x = graph.neighbors_directed(n, petgraph::Direction::Outgoing);
                print!("{:2?} ", x.map(|x| x.index()).collect::<Vec<_>>()); // direct targeted
            }
            println!();
            let dag = to_dag(&graph);
            println!("dag:");
            for n in dag.node_indices() {
                let x = dag.neighbors_directed(n, petgraph::Direction::Incoming);
                print!("{:2} ", x.count()); // direct targeted
                let x = dag.neighbors_directed(n, petgraph::Direction::Outgoing);
                println!("{:2?} ", x.map(|x| x.index()).collect::<Vec<_>>()); // direct targeted
            }
            println!();
            Some(dag)
        })
        .collect();

    graphs.push(singles);
    graphs
}

const UNINIT_INDEX: usize = usize::MAX;

fn from_cc<N: Clone, E: Clone + 'static>(
    graph: &petgraph::Graph<N, E>,
    map: &mut [petgraph::prelude::NodeIndex],
    x: Vec<petgraph::prelude::NodeIndex>,
) -> petgraph::Graph<N, E> {
    use petgraph::prelude::*;
    let uninit = NodeIndex::new(UNINIT_INDEX);

    let mut g = Graph::<_, _>::with_capacity(x.len(), x.len());

    x.iter().rev().for_each(|x| {
        map[x.index()] = g.add_node(graph[*x].clone());
    });

    x.iter().rev().for_each(|a| {
        graph.edges(*a).for_each(|b| {
            assert_ne!(a, &uninit);
            assert_eq!(b.source(), *a);
            assert_ne!(b.target(), uninit);
            g.add_edge(map[a.index()], map[b.target().index()], b.weight().clone());
        });
    });
    g
}

struct ToDag<'g, N: Clone, E: Clone + 'static, G> {
    graph: &'g petgraph::Graph<N, E>,
    pub toposort: Vec<petgraph::prelude::NodeIndex>,
    pub intermediate: petgraph::adj::List<(), petgraph::prelude::NodeIndex>,
    pub reduction: petgraph::adj::List<(), petgraph::prelude::NodeIndex>,
    pub new_graph: G,
    nodes_index: usize,
    nodes_index_edges: usize,
    nodes_index_succ: Vec<petgraph::prelude::NodeIndex>,
}

impl<'g, N: Clone, E: Clone + 'static, G: Default> ToDag<'g, N, E, G> {
    fn new(graph: &'g petgraph::Graph<N, E>, g: G) -> Self {
        use petgraph::prelude::*;
        let toposort = petgraph::algo::toposort(&graph, None).expect("acyclic");
        let (intermediate, revmap) =
            petgraph::algo::tred::dag_to_toposorted_adjacency_list(&graph, &toposort);
        let (reduction, _closure) =
            petgraph::algo::tred::dag_transitive_reduction_closure::<_, NodeIndex>(&intermediate);
        // let mut g = Graph::<_, _>::with_capacity(graph.node_count(), reduction.edge_count());
        ToDag {
            graph,
            toposort,
            intermediate,
            reduction,
            new_graph: g,
            nodes_index: 0,
            nodes_index_edges: 0,
            nodes_index_succ: vec![],
        }
    }
}
impl<'g, N: Clone, E: Clone + 'static, N2, E2> ToDag<'g, N, E, petgraph::Graph<N2, E2>> {
    fn advance_node(&mut self, f: impl Fn(N) -> N2) -> Option<&N2> {
        if self.nodes_index >= self.graph.node_count() {
            return None;
        }
        let w = f(self.graph[self.toposort[self.nodes_index]].clone());
        self.nodes_index += 1;
        let i = self.new_graph.add_node(w);
        self.new_graph.node_weight(i)
    }
    // can return None when node is not available
    // End when advance_edge returns after advance_node itself has returned None
    fn advance_edge(&mut self, f: impl Fn(E) -> E2) -> Option<&E2> {
        if self.nodes_index_edges.saturating_sub(1) >= self.reduction.node_count() {
            return None;
        }
        let Some(y) = self.nodes_index_succ.pop() else {
            self.nodes_index_edges += 1;
            let x = petgraph::graph::NodeIndex::new(self.nodes_index_edges - 1);
            if self.nodes_index_edges > self.reduction.node_count() {
                return None;
            }
            // let n = graph.neighbors_directed(a, dir)
            let mut e: Vec<_> = self
                .reduction
                .edge_indices_from(x)
                .filter_map(|e| self.reduction.edge_endpoints(e))
                .map(|x| x.1)
                .collect();
            if e.is_empty() {
                return self.advance_edge(f);
            }
            e.sort();
            e.dedup();
            for y in &e {
                print!("{}-{} ", x.index(), y.index());
            }
            println!();
            e.reverse();
            self.nodes_index_succ = e;
            return self.advance_edge(f);
        };
        let x = petgraph::graph::NodeIndex::new(self.nodes_index_edges - 1);

        // print!("{} {} --> ", x.index(), y.index());
        // println!(
        //     "{} {}",
        //     toposort[x.index()].index(),
        //     toposort[y.index()].index()
        // );
        let old_x = self.toposort[x.index()];
        assert!(old_x.index() < self.new_graph.node_count());
        let old_y = self.toposort[y.index()];
        assert!(old_y.index() < self.new_graph.node_count());
        let e = if let Some(e) = self.graph.find_edge(old_x, old_y) {
            e
        } else if let Some(e) = self.graph.find_edge(old_y, old_x) {
            e
        } else {
            unreachable!()
        };
        // TODO use neighbors slice
        let w = self.graph.edge_weight(e).unwrap().clone();
        let w = f(w);
        let i = self.new_graph.add_edge(y, x, w); // reversed
        self.new_graph.edge_weight(i)
    }
}

fn to_dag<N: Clone, E: Clone + 'static>(graph: &petgraph::Graph<N, E>) -> petgraph::Graph<N, E> {
    let mut td = ToDag::new(
        graph,
        petgraph::Graph::<N, E>::with_capacity(graph.node_count(), graph.edge_count()),
    );
    while let Some(x) = td.advance_node(|w| w) {}
    while let Some(x) = td.advance_edge(|w| w) {}
    td.new_graph
    // dbg!();
    // use petgraph::prelude::*;
    // let toposort = petgraph::algo::toposort(&graph, None).expect("acyclic");
    // let (intermediate, revmap) =
    //     petgraph::algo::tred::dag_to_toposorted_adjacency_list(&graph, &toposort);
    // let (reduction, _closure) =
    //     petgraph::algo::tred::dag_transitive_reduction_closure::<_, NodeIndex>(&intermediate);

    // let mut g = Graph::<_, _>::with_capacity(graph.node_count(), reduction.edge_count());
    // for i in intermediate.node_indices() {
    //     g.add_node(graph[toposort[i.index()]].clone());
    // }
    // println!("{:?}", intermediate);
    // println!(
    //     "{:?}",
    //     toposort.iter().map(|x| x.index()).collect::<Vec<_>>()
    // );
    // println!("{:?}", revmap.iter().map(|x| x.index()).collect::<Vec<_>>());
    // println!("toposorted:");
    // for x in intermediate.node_indices() {
    //     print!(
    //         "{:2?} ",
    //         reduction
    //             .edge_indices_from(x)
    //             .filter_map(|e| reduction.edge_endpoints(e))
    //             .map(|x| x.1.index())
    //             .collect::<Vec<_>>()
    //     );
    // }
    // println!();
    // println!("edge_indices_from:");
    // for x in intermediate.node_indices() {
    //     // let n = graph.neighbors_directed(a, dir)
    //     let mut e: Vec<_> = reduction
    //         .edge_indices_from(x)
    //         .filter_map(|e| reduction.edge_endpoints(e))
    //         .collect();
    //     e.sort();
    //     e.dedup();
    //     for (x, y) in &e {
    //         print!("{}-{} ", x.index(), y.index());
    //     }
    //     println!();
    //     for (_x, y) in e {
    //         assert_eq!(x, _x);
    //         // print!("{} {} --> ", x.index(), y.index());
    //         // println!(
    //         //     "{} {}",
    //         //     toposort[x.index()].index(),
    //         //     toposort[y.index()].index()
    //         // );
    //         let e = graph
    //             .find_edge(toposort[x.index()], toposort[y.index()])
    //             .unwrap_or_else(|| {
    //                 graph
    //                     .find_edge(toposort[y.index()], toposort[x.index()])
    //                     .unwrap()
    //             }); // TODO use neighbors slice
    //         let w = graph.edge_weight(e).unwrap().clone();
    //         g.add_edge(y, x, w);
    //     }
    // }
    // g
}

#[derive(Debug, enumset::EnumSetType)]
pub enum SimpKind {
    Init,
    Uniqs,
    RMall,
    RMs,
    SimpEQ,
    Focus,
}

pub fn each<E, I>(
    tr: &hyperast_gen_ts_tsquery::code2query::TR<E, I>,
    mut f: impl FnMut(SimpKind, &E),
    mut g: impl FnMut(SimpKind, &I),
) {
    use hyperast_gen_ts_tsquery::code2query::TR;
    match tr {
        TR::Init(t) => f(SimpKind::Init, t),
        TR::Uniqs(t) => g(SimpKind::Uniqs, t),
        TR::RMall(t) => g(SimpKind::RMall, t),
        TR::RMs(t) => g(SimpKind::RMs, t),
        TR::SimpEQ(t) => g(SimpKind::SimpEQ, t),
        TR::Focus(t) => g(SimpKind::Focus, t),
    }
}

fn compress_webgraphs(g: Vec<petgraph::Graph<IdN, ()>>) -> Vec<_G> {
    for (i, g) in g.into_iter().enumerate() {
        println!("lattice {i} compression:");
        compress_webgraph(g);
    }
    vec![]
}
fn compress_webgraph(g: petgraph::Graph<IdN, ()>) {
    super::graph_compression::compress(g);
}

struct ExIt<'g> {
    content: &'g _G,
    ex_curr: u32,
    ex_offset: usize,
    ex_pretty: u32,
    it: std::ops::Range<usize>,
}
#[derive(Clone, Copy)]
struct ExSet(u64);
impl ExSet {
    fn len(&self) -> u32 {
        self.0.count_ones()
    }
}
impl<'a> ExIt<'a> {
    fn new(content: &'a _G) -> Self {
        Self {
            ex_curr: 0,
            ex_offset: 1,
            ex_pretty: 0,
            it: (0..content.queries.len()),
            content,
        }
    }
}
impl Iterator for ExIt<'_> {
    type Item = ExSet;
    fn next(&mut self) -> Option<Self::Item> {
        let i = self.it.next()?;
        let mut inits = 0u64;
        while (self.ex_curr as usize) < i && self.ex_offset < self.content.ex_query.len() {
            self.ex_pretty += 1;
            assert!((self.ex_pretty as usize) < self.content.ex_pretty.len());
            self.ex_curr += self.content.ex_query[self.ex_offset];
            self.ex_offset += 1;
        }
        while self.ex_curr as usize == i && self.ex_offset < self.content.ex_query.len() {
            inits |= 1 << self.ex_pretty;
            self.ex_pretty += 1;
            assert!((self.ex_pretty as usize) < self.content.ex_pretty.len());
            self.ex_curr += self.content.ex_query[self.ex_offset];
            self.ex_offset += 1;
        }
        Some(ExSet(inits))
    }
}

#[test]
fn test_ex_compression_decompression() {
    let mut _ex_query = vec![0, 0, 4, 6, 40, 41, 41, 41, 41, 100, 100, 102, 103];
    let mut ex_query = _ex_query.clone();
    let mut x = 0u32;
    for y in ex_query.iter_mut() {
        let r = *y - x;
        x = *y;
        *y = r;
    }
    let mut ex_curr = 0;
    let mut ex_offset = 0;
    let mut decomp = vec![];
    let mut _decomp = vec![];
    for i in 0..120 {
        let mut inits = 0;
        loop {
            if ex_curr as usize == i {
                inits |= 1 << ex_offset;
                _decomp.push(i as u32);
                ex_offset += 1;
                if ex_offset >= ex_query.len() {
                    break;
                }
                ex_curr += ex_query[ex_offset];
            } else if (ex_curr as usize) < i {
                if ex_offset >= ex_query.len() {
                    break;
                }
                ex_curr += ex_query[ex_offset];
            } else {
                break;
            }
        }
        decomp.push(inits);
    }
    assert_eq!(_ex_query, _decomp);
}
