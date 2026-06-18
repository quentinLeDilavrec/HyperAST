use hyperast::hashed::NodeHashs;
use hyperast::store::nodes::{Compo, GatherAttrErazed};
use hyperast::tree_gen::SubTreeMetrics;

/// Identifying elements and fundamental derived metrics used to accelerate deduplication.
/// For example, hashing subtrees accelerates the deduplication process,
/// but it requires to hash children and it can be done by accumulating hashes iteratively per child (see [`hyperast::hashed::inner_node_hash`]).
pub struct BasicDirAcc<Id, L, M> {
    pub name: String,
    pub children: Vec<Id>,
    pub children_names: Vec<L>,
    pub metrics: M,
}

impl<Id, L, M: Default> BasicDirAcc<Id, L, M> {
    pub fn new(name: String) -> Self {
        Self {
            name,
            children_names: Default::default(),
            children: Default::default(),
            metrics: Default::default(),
        }
    }
}

impl<Id, L, U: NodeHashs> BasicDirAcc<Id, L, SubTreeMetrics<U>> {
    pub fn push(&mut self, name: L, id: Id, metrics: SubTreeMetrics<U>) {
        self.children.push(id);
        self.children_names.push(name);
        self.metrics.acc(metrics);
    }
}

impl<Id, L, M> BasicDirAcc<Id, L, M> {
    pub fn map_metrics<N>(self, f: impl Fn(M) -> N) -> BasicDirAcc<Id, L, N> {
        BasicDirAcc {
            metrics: f(self.metrics),
            name: self.name,
            children: self.children,
            children_names: self.children_names,
        }
    }
}

impl<Id, L, M> BasicDirAcc<Id, L, M> {
    pub fn persist<K>(
        self,
        dyn_builder: &mut impl GatherAttrErazed,
        interned_kind: K,
        label_id: L,
    ) -> M
    where
        K: Compo,
        L: Compo,
        Id: 'static + Send + Sync,
    {
        dyn_builder.add(interned_kind);
        dyn_builder.add(label_id);

        let children = self.children;
        let children_names = self.children_names;
        assert_eq!(children_names.len(), children.len());
        use hyperast::store::nodes::compo;
        if children.len() == 1 {
            dyn_builder.add(compo::CS(children_names.into_boxed_slice()));
            let Ok(cs) = children.try_into() else {
                unreachable!();
            };
            dyn_builder.add(compo::CS0::<_, 1>(cs));
        } else if children.len() == 2 {
            dyn_builder.add(compo::CS(children_names.into_boxed_slice()));
            let Ok(cs) = children.try_into() else {
                unreachable!();
            };
            dyn_builder.add(compo::CS0::<_, 2>(cs));
        } else if !children.is_empty() {
            use compo::CS;
            dyn_builder.add(CS(children_names.into_boxed_slice()));
            dyn_builder.add(CS(children.into_boxed_slice()));
        }
        self.metrics
    }
}
