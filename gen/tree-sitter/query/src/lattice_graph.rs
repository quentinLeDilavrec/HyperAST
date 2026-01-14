use num::ToPrimitive;
use petgraph::csr::Csr;
use petgraph::{Directed, Graph};

use hashbrown::{HashMap, HashSet};
use petgraph::graph::NodeIndex;
use petgraph::visit::{EdgeRef, IntoNodeReferences};

use crate::code2query::QueryLattice;

type IdN = hyperast::store::defaults::NodeIdentifier;
type Latt<'a, P> = &'a QueryLattice<&'a P>;

pub fn make_poset_graph<N, E: 'static, P: PartialEq>(
    lattice: &QueryLattice<P>,
    f: impl Fn(IdN) -> N,
    g: impl Fn(petgraph::graph::NodeIndex, petgraph::graph::NodeIndex, crate::code2query::TrMarker) -> E,
) -> petgraph::acyclic::Acyclic<Graph<N, E>> {
    use petgraph::prelude::*;
    let mut graph: DiGraph<_, _> = Default::default();

    let mut q_map = HashMap::<IdN, NodeIndex>::with_capacity(lattice.queries.len());
    for i in 0..lattice.queries.len() {
        let k = lattice.queries[i].0;
        let t = f(k);
        let index = graph.add_node(t);
        let _ = q_map.insert(k, index);
    }
    let mut graph = petgraph::acyclic::Acyclic::try_from_graph(graph).unwrap();

    (lattice.raw_rels.iter())
        .filter_map(|(k, v)| q_map.get(k).map(|source| (*source, v)))
        .for_each(|(source, v)| {
            v.iter().for_each(|v| {
                v.each(
                    |_, _| (),
                    |kind, target| {
                        let Some(&target) = q_map.get(target) else {
                            return;
                        };
                        let e = g(source, target, kind);
                        graph
                            .try_add_edge(source, target, e)
                            .expect(&format!("{:?}", kind));
                    },
                )
            });
        });
    graph
}
pub trait EdgeUnion {
    fn union(&mut self, rhs: Self);
}
impl<T: enumset::EnumSetType> EdgeUnion for enumset::EnumSet<T> {
    fn union(&mut self, rhs: Self) {
        *self |= rhs;
    }
}
impl EdgeUnion for () {
    fn union(&mut self, _rhs: Self) {}
}
pub fn group_lattices<N: Clone, E: Clone + 'static + EdgeUnion>(
    graph: petgraph::acyclic::Acyclic<Graph<N, E>>,
) -> Vec<Graph<N, E, petgraph::Directed, petgraph::csr::DefaultIx>> {
    use petgraph::prelude::*;

    use petgraph::visit::NodeIndexable;
    let mut vertex_sets = petgraph::unionfind::UnionFind::new(graph.node_bound());
    for edge in graph.edge_references() {
        let (a, b) = (edge.source(), edge.target());

        // union the two vertices of the edge
        vertex_sets.union(graph.to_index(a), graph.to_index(b));
    }
    let labels = vertex_sets.into_labeling();
    // dbg!(&labels);
    let mut sorting: Vec<_> = (0..labels.len()).collect();
    sorting.sort_unstable_by_key(|i| labels[*i]);
    // dbg!(&sorting.iter().map(|i| labels[*i]).collect::<Vec<_>>());
    sorting.dedup_by_key(|i| labels[*i]);

    let scc = sorting
        .into_iter()
        .map(|i| labels[i])
        .map(|i| {
            labels
                .iter()
                .enumerate()
                .filter_map(|(l, j)| (i == *j).then_some(NodeIndex::new(l)))
                .collect::<Vec<_>>()
        })
        .collect::<Vec<_>>();

    log::error!("scc {}", scc.len());

    let uninit = NodeIndex::new(usize::MAX);
    let mut map: Vec<NodeIndex> = vec![uninit; graph.node_count()];

    let mut singles = Graph::new();
    let mut graphs: Vec<_> = scc
        .into_iter()
        .filter_map(|x| {
            if x.len() == 1 {
                singles.add_node(graph.node_weight(x[0]).unwrap().clone());
                return None;
            }
            let mut g = Graph::<_, E>::with_capacity(x.len(), x.len());

            x.iter().rev().for_each(|x| {
                map[x.index()] = g.add_node(graph.node_weight(*x).unwrap().clone());
            });

            x.iter().for_each(|a| {
                graph.edges(*a).for_each(|b| {
                    assert_ne!(a, &uninit);
                    assert_eq!(b.source(), *a);
                    assert_ne!(b.target(), uninit);
                    if let Some(edge) = g.find_edge(map[a.index()], map[b.target().index()]) {
                        g[edge].union(b.weight().clone());
                    } else {
                        g.add_edge(map[b.target().index()], map[a.index()], b.weight().clone());
                    }
                });
            });

            let graph = g;
            let graph = petgraph::acyclic::Acyclic::try_from_graph(graph).unwrap();

            let toposort = petgraph::algo::toposort(&graph, None).unwrap();

            let (intermediate, _revmap) =
                petgraph::algo::tred::dag_to_toposorted_adjacency_list(&graph, &toposort);
            let (reduction, _closure) = petgraph::algo::tred::dag_transitive_reduction_closure::<
                _,
                NodeIndex,
            >(&intermediate);

            // let mut graph = graph.into_inner();

            // graph.retain_edges(|x, y| {
            //     if let Some((x, y)) = x.edge_endpoints(y) {
            //         reduction.contains_edge(revmap[x.index()], revmap[y.index()])
            //     } else {
            //         false
            //     }
            // });
            let mut g: Graph<N, E> =
                Graph::with_capacity(graph.node_count(), reduction.edge_count());
            for n in &toposort {
                g.add_node(graph[*n].clone());
            }
            for e in reduction.edge_indices() {
                if let Some((u, v)) = reduction.edge_endpoints(e) {
                    g.add_edge(
                        v,
                        u,
                        graph
                            .edge_weight(
                                graph
                                    .find_edge_undirected(toposort[v.index()], toposort[u.index()])
                                    .unwrap()
                                    .0,
                            )
                            .unwrap()
                            .clone(),
                    );
                }
            }
            log::trace!("q count {}", g.node_count());
            Some(g)
        })
        .collect();

    graphs.push(singles);
    graphs
}

pub struct GroupedLattices<Q, E = crate::code2query::TrMarkers> {
    pub graphs: Vec<(LatticeStats, Graph<Q, E>)>,
}

impl<Q: hyperast::position::position_accessors::SolvedPosition<IdN> + Clone + From<IdN>>
    GroupedLattices<Q>
{
    pub fn new<P: PartialEq>(lattice: Latt<'_, P>) -> Self {
        // preps
        let graph = make_poset_graph(
            lattice,
            |query| query.into(),
            |_source, _target, kind| crate::code2query::TrMarkers::from(kind),
        );
        let graphs = group_lattices(graph);
        let mut graphs: Vec<_> = graphs
            .into_iter()
            .map(|graph| {
                (
                    lattice_stats(lattice, &graph, |x| {
                        x.contains(crate::code2query::TrMarker::Uniqs)
                    }),
                    graph,
                )
            })
            .collect();
        graphs.sort_by(|a, b| a.0.cmp(&b.0).reverse());
        GroupedLattices { graphs }
    }
}

pub struct LatticeStats {
    leaf_count: usize,
    node_count: usize,
    edge_count: usize,
    pub complete_tops: Vec<(NodeIndex, TopStats)>,
    pub incompletes_tops: Vec<(NodeIndex, TopStats)>,
    pub uniqs: hashbrown::HashSet<NodeIndex>,
}

impl Eq for LatticeStats {}
impl PartialEq for LatticeStats {
    fn eq(&self, other: &Self) -> bool {
        self.leaf_count.eq(&other.leaf_count)
            && self.node_count.eq(&other.node_count)
            && self.edge_count.eq(&other.edge_count)
    }
}

impl Ord for LatticeStats {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        self.partial_cmp(other).unwrap_or(std::cmp::Ordering::Equal)
    }
}
impl PartialOrd for LatticeStats {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(
            self.complete_tops
                .cmp(&other.complete_tops)
                .then(self.leaf_count.cmp(&other.leaf_count))
                .then(self.node_count.cmp(&other.node_count))
                .then(self.edge_count.cmp(&other.edge_count)),
        )
    }
}
impl std::fmt::Display for LatticeStats {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{} | {} | {} | {} | {}",
            self.node_count,
            self.edge_count,
            self.leaf_count,
            self.complete_tops.len() + self.incompletes_tops.len(),
            self.incompletes_tops.len(),
        )
    }
}

impl LatticeStats {
    pub fn header() -> &'static str {
        "nodes | edges | leafs | tops | incomplete tops"
    }
}

pub fn lattice_stats<Q: hyperast::position::position_accessors::SolvedPosition<IdN>, E, P>(
    lattice: &QueryLattice<P>,
    graph: &Graph<Q, E>,
    is_uniq: fn(&E) -> bool,
) -> LatticeStats {
    let uniqs: hashbrown::HashSet<_> = graph
        .externals(petgraph::Direction::Outgoing)
        .flat_map(|x| {
            let mut uniqs = graph
                .edges_directed(x, petgraph::Direction::Incoming)
                .filter(|x| is_uniq(x.weight()))
                .map(|x| x.source())
                .peekable();
            if uniqs.peek().is_none() {
                uniqs.chain(vec![x].into_iter())
            } else {
                uniqs.chain(vec![].into_iter())
            }
        })
        .collect();

    let tops = graph.externals(petgraph::Direction::Incoming);

    let (mut incompletes, mut tops_ranked): (Vec<_>, Vec<_>) = tops
        .map(|node_id| {
            (
                node_id,
                top_stats(graph, &lattice.query_store, &uniqs, node_id),
            )
        })
        .partition(|x| x.1.incomplete);
    incompletes.sort_by(|a, b| a.1.cmp(&b.1).reverse());
    tops_ranked.sort_by(|a, b| a.1.cmp(&b.1).reverse());

    LatticeStats {
        leaf_count: graph
            .node_references()
            .flat_map(|(_, n)| lattice.raw_rels.get(&n.node()).unwrap())
            .filter_map(|x| {
                let crate::code2query::TR::Init(x) = x else {
                    return None;
                };
                Some(x)
            })
            .count(),
        node_count: graph.node_count(),
        edge_count: graph.edge_count(),
        complete_tops: tops_ranked,
        incompletes_tops: incompletes,
        uniqs,
    }
}

pub struct TopStats {
    pub paths: usize,
    pub reachable_uniqs: hashbrown::HashSet<NodeIndex>,
    pub patt_stats: PatternStats,
    pub incomplete: bool,
}
impl Eq for TopStats {}
impl PartialEq for TopStats {
    fn eq(&self, other: &Self) -> bool {
        self.paths.eq(&other.paths)
            && self.incomplete.eq(&other.incomplete)
            && self.reachable_uniqs.len().eq(&other.reachable_uniqs.len())
            && self.patt_stats.eq(&other.patt_stats)
    }
}

impl Ord for TopStats {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        self.partial_cmp(other).unwrap_or(std::cmp::Ordering::Equal)
    }
}
impl PartialOrd for TopStats {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(
            self.paths
                .cmp(&other.paths)
                .then(self.incomplete.cmp(&other.incomplete))
                .then(self.reachable_uniqs.len().cmp(&other.reachable_uniqs.len()))
                .then(self.patt_stats.cmp(&other.patt_stats)),
        )
    }
}
impl std::fmt::Display for TopStats {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if f.alternate() {
            writeln!(f, "* paths: {}", self.paths)?;
            writeln!(f, "* reachable uniqs: {}", self.reachable_uniqs.len())?;
            Ok(())
        } else {
            write!(
                f,
                "{} | {} | {} | {}",
                self.paths,
                self.reachable_uniqs.len(),
                self.patt_stats,
                self.incomplete
            )
        }
    }
}

impl TopStats {
    pub fn iter_covering_all<'a, It: Iterator<Item = (NodeIndex, &'a TopStats)>>(
        it: It,
        uniqs: &'a mut HashSet<NodeIndex>,
    ) -> impl Iterator<Item = (NodeIndex, &'a TopStats)> {
        struct Iter<'a, It> {
            inner: It,
            uniqs: &'a mut HashSet<NodeIndex>,
        }

        impl<'a, It> Iterator for Iter<'a, It>
        where
            It: Iterator<Item = (NodeIndex, &'a TopStats)>,
        {
            type Item = (NodeIndex, &'a TopStats);

            fn next(&mut self) -> Option<Self::Item> {
                if self.uniqs.is_empty() {
                    return None;
                }
                let (node_id, stats) = self.inner.next()?;
                let mut useful = false;
                for uniq in &stats.reachable_uniqs {
                    useful |= self.uniqs.remove(uniq);
                }
                if !useful {
                    return None;
                }
                Some((node_id, stats))
            }
        }
        Iter { inner: it, uniqs }
    }
}

fn top_stats<Q: hyperast::position::position_accessors::SolvedPosition<IdN>, E>(
    lattice: &Graph<Q, E>,
    stores: &hyperast::store::SimpleStores<crate::types::TStore>,
    uniqs: &HashSet<NodeIndex>,
    node_id: NodeIndex,
) -> TopStats {
    let patt_id = lattice.node_weight(node_id).unwrap().node();
    let patt_stats = pattern_stats(&stores, patt_id);

    let mut visit = vec![node_id];
    let mut paths = 0;
    let mut reachable_uniqs = hashbrown::HashSet::new();
    while let Some(node_id) = visit.pop() {
        if uniqs.contains(&node_id) {
            reachable_uniqs.insert(node_id);
        }
        for e in lattice.edges(node_id) {
            paths += 1;
            visit.push(e.target());
        }
    }

    let meta_simp = hyperast_tsquery::Query::new(
        r#"
        (named_node
            (identifier) (#EQ? "argument_list")
            (named_node
                (identifier) (#EQ? "identifier")
            ) .
            (predicate
                (identifier) (#EQ? "EQ")
                (parameters
                    (string) @label
                )
            ) @incomplete
        )
        (named_node
            (identifier) (#EQ? "field_access") .
            (named_node
                (identifier) (#EQ? "identifier")
            ) .
            (predicate
                (identifier) (#EQ? "EQ")
                (parameters
                    (string) @label
                )
            ) @incomplete
        )
        (named_node
            (identifier) (#EQ? "array_access") .
            (named_node
                (identifier) (#EQ? "identifier")
            ) .
            (predicate
                (identifier) (#EQ? "EQ")
                (parameters
                    (string) @label
                )
            ) @incomplete
        )
        "#,
        crate::language(),
    )
    .unwrap();
    let cid = meta_simp.capture_index_for_name("incomplete").unwrap();
    let m = crate::code2query::find_matches(&stores, patt_id, &meta_simp, cid);
    TopStats {
        paths,
        patt_stats,
        reachable_uniqs,
        incomplete: !m.is_empty(),
    }
}

pub struct PatternStats {
    byte_len: usize,
    node_count: usize,
    pred_count: usize,
    height: usize,
}
impl Eq for PatternStats {}
impl PartialEq for PatternStats {
    fn eq(&self, other: &Self) -> bool {
        self.byte_len.eq(&other.byte_len)
            && self.node_count.eq(&other.node_count)
            && self.pred_count.eq(&other.pred_count)
            && self.height.eq(&other.height)
    }
}

impl Ord for PatternStats {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        self.partial_cmp(other).unwrap_or(std::cmp::Ordering::Equal)
    }
}
impl PartialOrd for PatternStats {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(
            self.byte_len
                .cmp(&other.byte_len)
                .then(self.node_count.cmp(&other.node_count))
                .then(self.pred_count.cmp(&other.pred_count))
                .then(self.height.cmp(&other.height)),
        )
    }
}
impl std::fmt::Display for PatternStats {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if f.alternate() {
            writeln!(f, "* node count: {}", self.node_count)?;
            writeln!(f, "* byte length: {}", self.byte_len)?;
            Ok(())
        } else {
            write!(
                f,
                "{} | {} | {} | {}",
                self.byte_len, self.node_count, self.pred_count, self.height
            )
        }
    }
}
impl PatternStats {
    pub fn header(&self) -> &'static str {
        "| bytes | size | preds | height |
|------|------|------|------|"
    }
}

pub fn pattern_stats(
    stores: &hyperast::store::SimpleStores<crate::types::TStore>,
    id: IdN,
) -> PatternStats {
    use hyperast::types::{WithSerialization, WithStats};
    let n = stores.node_store.resolve(id);
    PatternStats {
        byte_len: n.try_bytes_len().unwrap(),
        node_count: n.size(),
        pred_count: 0,
        height: n.height(),
    }
}

type IdNQ = IdN;
pub struct Prep<'a, P, E = crate::code2query::TrMarkers> {
    pub g: Vec<Graph<IdNQ, E>>,
    lattice: &'a QueryLattice<P>,
}

impl<'a, P> Prep<'a, P> {
    pub fn extract_and_group(lattice: &'a QueryLattice<P>) -> Self
    where
        P: PartialEq + Clone,
    {
        dbg!();
        // for (i, q) in lattice.queries.iter().enumerate() {
        //     print!("{i} {} ", q.1.len());
        //     println!(
        //         "{:?}",
        //         (lattice.raw_rels.get(&q.0).unwrap().iter())
        //             .filter_map(|x| x.no_init())
        //             .map(|x| lattice.queries.iter().position(|y| y.0 == *x).unwrap())
        //             .collect::<Vec<_>>()
        //     );
        // }
        // dbg!(lattice.queries.iter().map(|x| &x.1).collect::<Vec<_>>());
        // NOTE at this point is it not a lattice just a poset
        let graph = make_poset_graph(
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
            |_, _, e| crate::code2query::TrMarkers::from(e),
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

    pub fn wcc_description(&self, i: usize) -> impl std::fmt::Display
    where
        P: PartialEq + Clone + Into<IdN>,
    {
        struct PP<'a, P>(&'a Prep<'a, P>, usize);
        impl<P> std::fmt::Display for PP<'_, P> {
            fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
                write!(
                    f,
                    "q count {} init count {}",
                    self.0.g[self.1].node_count(),
                    self.0.g[self.1]
                        .node_indices()
                        .filter(|x| self.0.g[self.1]
                            .neighbors_directed(*x, petgraph::Direction::Outgoing)
                            .count()
                            > 0)
                        .count()
                )
            }
        }
        PP(self, i)
    }

    pub fn tops(&self) -> impl Iterator<Item = (String, Vec<u32>)> {
        self.g
            .iter()
            .flat_map(move |g| {
                let mut dedup = hashbrown::HashSet::new();
                tops(g)
                    .map(|x| g[x])
                    .filter(move |x| dedup.insert(x.clone()))
            })
            .map(|top| (self.lattice.pretty(&top), self.lattice.extract2(top).1))
    }

    pub fn tops_plus_one(&self) -> impl Iterator<Item = (String, Vec<u32>)> {
        use petgraph::Direction::Outgoing as Down;
        fn tops_plus_one<E>(
            g: &Graph<IdNQ, E>,
        ) -> impl Iterator<Item = petgraph::prelude::NodeIndex> {
            tops(g).flat_map(|x| [x].into_iter().chain(g.neighbors_directed(x, Down)))
        }
        self.g
            .iter()
            .flat_map(move |g| {
                let mut dedup = hashbrown::HashSet::new();
                tops_plus_one(g)
                    .map(|x| g[x])
                    .filter(move |x| dedup.insert(x.clone()))
            })
            .map(|top| (self.lattice.pretty(&top), self.lattice.extract2(top).1))
    }

    pub fn majors(&self) -> impl Iterator<Item = (String, Vec<u32>)> {
        use petgraph::Direction::Outgoing as Down;
        self.g
            .iter()
            .flat_map(move |g| {
                let mut dedup = hashbrown::HashSet::new();
                tops(g)
                    .flat_map(|x| [x].into_iter().chain(g.neighbors_directed(x, Down)))
                    .map(|x| g[x])
                    .filter(move |x| dedup.insert(x.clone()))
            })
            .map(|maj| (self.lattice.pretty(&maj), self.lattice.extract2(maj).1))
    }
}
fn tops<E>(g: &Graph<IdNQ, E>) -> impl Iterator<Item = petgraph::prelude::NodeIndex> {
    use petgraph::Direction::Incoming as Up;
    g.node_indices()
        .filter(|x| g.neighbors_directed(*x, Up).count() == 0)
}

fn describe_lattice<P: PartialEq + Clone, E: Clone>(
    lattice: &Graph<IdN, E>,
    poset: &QueryLattice<P>,
    i: usize,
) {
    println!("lattice {i} description:");
    print_lattice_shape(lattice, poset);
    print_lattice_extra(lattice, poset);
}

fn print_lattice_shape<P, E>(lattice: &Graph<legion::Entity, E>, poset: &QueryLattice<P>) {
    for i in lattice.node_indices() {
        print!("{} {:?}", i.index(), idn_to_u64(lattice[i]));
        print!(
            " {:?}", // inits
            (poset.raw_rels.get(&lattice[i]).unwrap().iter())
                .filter_map(|x| x.as_init())
                .map(|_| 1)
                .collect::<Vec<_>>()
        );
        print!(
            "{:?}", // spe in poset
            (poset.raw_rels.get(&lattice[i]).unwrap().iter())
                .filter_map(|x| x.no_init().map(|x| idn_to_u64(*x)))
                .collect::<Vec<_>>()
        );
        println!(
            "{:?}", // spe in lattice
            lattice
                .neighbors_directed(i, petgraph::Direction::Outgoing)
                .map(|j| idn_to_u64(*lattice.node_weight(j).unwrap()))
                .collect::<Vec<_>>()
        );
    }
}

fn print_lattice_extra<P: PartialEq + Clone, E: Clone>(
    lattice: &Graph<legion::Entity, E>,
    poset: &QueryLattice<P>,
) {
    if let Ok(numbers) = compute_topo_numbers::<P, u64, E>(lattice, poset, 64) {
        dbg!();
        print_topo_numbers(lattice, poset, numbers, |x| num::PrimInt::count_ones(*x));
    } else if let Ok(numbers) = compute_topo_numbers::<P, u128, E>(lattice, poset, 128) {
        dbg!();
        print_topo_numbers(lattice, poset, numbers, |x| num::PrimInt::count_ones(*x));
    } else if let Ok(numbers) = compute_topo_numbers::<P, num::BigUint, E>(lattice, poset, 256 * 64)
    {
        dbg!();
        print_topo_numbers(lattice, poset, numbers, |x| {
            num::BigUint::count_ones(x) as u32
        });
    } else {
        dbg!();
        for n in lattice.node_indices() {
            let x = lattice.neighbors_directed(n, petgraph::Direction::Outgoing);
            print!("{:2} ", x.count()); // direct targeted
        }
        println!();
        for n in lattice.node_indices() {
            let x = lattice.neighbors_directed(n, petgraph::Direction::Incoming);
            print!("{:2} ", x.count()); // direct targeting
        }
        println!();
        log::warn!("too many inits for other metrics")
    }
}

fn compute_topo_numbers<P, Bitset, E: Clone>(
    g: &Graph<IdN, E>,
    lattice: &QueryLattice<P>,
    max: u32,
) -> Result<TopoNumbers<Bitset>, usize>
where
    P: PartialEq + Clone,
    Bitset: num::One + Clone,
    Bitset: std::ops::Shl<usize, Output = Bitset>,
    Bitset: std::ops::BitOrAssign + std::ops::BitOrAssign,
{
    TopoNumbers::compute(g, lattice, max).map_err(|x| x.dense_inits_counts.len())
}

fn print_topo_numbers<P, Bitset, E>(
    g: &Graph<IdN, E>,
    lattice: &QueryLattice<P>,
    numbers: TopoNumbers<Bitset>,
    count_ones: impl Fn(&Bitset) -> u32,
) where
    P: PartialEq,
    Bitset: num::One + num::Zero + Clone,
    Bitset: Eq,
    Bitset: std::ops::Shl<usize, Output = Bitset>,
    Bitset: std::ops::BitAnd<Bitset, Output = Bitset>,
{
    use hyperast::types::WithStats;
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
    for (qid, c, s) in snd_lvl {
        println!("---snd-----inits:{c}/{ex}-----size:{s}------",);
        if c > 1 {
            println!(
                "{}",
                lattice.pretty(&qid).lines().take(5).collect::<String>()
            );
        }
    }
}

fn idn_to_u64(x: IdN) -> u64 {
    unsafe { std::mem::transmute::<_, _>(x) }
}

struct TopoNumbers<Bitset> {
    // NOTE always sequential iteration (for now at least)
    // TODO try to use some compression, like EF ?
    /// indices over dense_inits_counts
    dense_inits_map: Vec<Imap>,
    /// number of instances per init pattern
    dense_inits_counts: Vec<usize>,
    /// inits per node, indices over dense_inits_map
    reachable_inits: ReachableInits<Bitset>,
}

type Imap = u8;

impl<Bitset> TopoNumbers<Bitset>
where
    Bitset: num::One + Clone,
    Bitset: std::ops::Shl<usize, Output = Bitset>,
    Bitset: std::ops::BitOrAssign + std::ops::BitOrAssign,
{
    fn compute<P, E: Clone>(
        g: &Graph<IdN, E>,
        lattice: &QueryLattice<P>,
        max: u32,
    ) -> Result<TopoNumbers<Bitset>, PreparedInits>
    where
        P: PartialEq + Clone,
    {
        for i in 0..g.node_count() {
            let n = petgraph::prelude::NodeIndex::new(i);
            eprint!("|{:>3} {} ", i, idn_to_u64(*g.node_weight(n).unwrap()));
            for j in g.neighbors_directed(n, petgraph::Direction::Outgoing) {
                eprint!("{}:{},", j.index(), idn_to_u64(*g.node_weight(j).unwrap()));
            }
            eprintln!();
        }
        let csr = graph_to_csr(&g);
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
                .filter_map(|x| x.as_init().cloned())
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
            if i == &u8::MAX && csr.node_count() < 20 {
                panic!("no init")
            } else if i == &u8::MAX {
                log::error!("no init");
            }
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

pub fn graph_to_csr<N: Clone, E: Clone>(g: &Graph<N, E, Directed>) -> Csr<N, E, Directed> {
    // let mut csr: Csr<(), _, Directed> = Csr::with_nodes(g.node_count());
    let mut csr: Csr<_, _, Directed> = Csr::new();
    g.node_weights().for_each(|n| {
        csr.add_node(n.clone());
    });

    for edge in g.edge_references() {
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

struct PreparedInits {
    dense_inits_map: Vec<Imap>,
    dense_inits_counts: Vec<usize>,
}
impl PreparedInits {
    fn compute<E>(csr: &Csr<IdN, E>, inits: impl Fn(IdN) -> usize) -> Self {
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
    fn retrieve(&mut self, _preped: &PreparedInits, i: IdG) -> D {
        self.0[i]
    }
    fn diffuse(&mut self, j: IdG, curr: &D) {
        self.0[j] = self.0[j].max(*curr + D::one());
    }
}

/// index in graph
type IdG = usize;

fn iter_topo<E>(csr: &Csr<IdN, E>) -> impl Iterator<Item = (IdG, IdN)> {
    (0..csr.node_count().to_u32().expect("use a larger Ix")).map(|i| (i as usize, csr[i]))
}

fn iter_succ<E>(csr: &Csr<IdN, E>, i: IdG) -> impl Iterator<Item = IdG> {
    use petgraph::visit::IntoNeighbors;
    csr.neighbors(i as u32).map(|i| i as usize)
}
