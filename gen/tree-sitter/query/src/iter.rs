use hyperast::position::{TreePath, TreePathMut};
use hyperast::store::nodes::legion::NodeIdentifier;
use hyperast::types::{AAAA, NodeStore, TypedHyperAST};
use hyperast::types::{Childrn, HyperAST, NodeId, Tree, TypedNodeStore, WithChildren};
use num::ToPrimitive;
use std::fmt;

use crate::types::TIdN;

pub struct IterAll<'a, T, HAST> {
    stores: &'a HAST,
    path: T,
    stack: Vec<(Id<NodeIdentifier>, u16, Option<Vec<NodeIdentifier>>)>,
}

enum Id<IdN> {
    Query(TIdN<IdN>),
    Other(IdN),
}

impl<IdN: Clone + Eq + AAAA> Id<IdN> {
    fn id(&self) -> &IdN {
        match self {
            Id::Query(node) => node.as_id(),
            Id::Other(node) => node,
        }
    }
}

impl<T: TreePath<NodeIdentifier, u16>, HAST> fmt::Debug for IterAll<'_, T, HAST> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("IterAllNodes")
            // .field("parents", &self.parents())
            .finish()
    }
}

impl<'a, T: TreePath<NodeIdentifier, u16>, HAST: HyperAST<IdN = NodeIdentifier>>
    IterAll<'a, T, HAST>
where
    HAST::NS: TypedNodeStore<TIdN<HAST::IdN>>,
{
    pub fn new(stores: &'a HAST, path: T, root: NodeIdentifier) -> Self {
        let root = if let Some(tid) = TypedNodeStore::try_typed(stores.node_store(), &root) {
            Id::Query(tid)
        } else {
            Id::Other(root)
        };
        let stack = vec![(root, 0, None)];
        Self {
            stores,
            path,
            stack,
        }
    }
}

impl<
    T: TreePathMut<NodeIdentifier, u16> + Clone + fmt::Debug,
    HAST: TypedHyperAST<TIdN<NodeIdentifier>, Idx = u16>,
> Iterator for IterAll<'_, T, HAST>
{
    type Item = T;

    fn next(&mut self) -> Option<Self::Item> {
        loop {
            let (node, offset, children) = self.stack.pop()?;
            if let Some(children) = children {
                if offset.to_usize().unwrap() < children.len() {
                    let child = children[offset.to_usize().unwrap()];
                    self.path.check(self.stores).unwrap();
                    {
                        let b = self.stores.node_store().resolve(node.id());
                        if b.has_children() {
                            assert!(offset < b.child_count());
                            let cs = b.children().unwrap();
                            assert_eq!(child, cs[num::cast(offset).unwrap()]);
                        } else {
                            panic!()
                        }
                    }
                    if offset == 0 {
                        if let Some(x) = self.path.node() {
                            assert_eq!(x, node.id())
                        }
                        self.path.goto(child, offset);
                        self.path.check(self.stores).unwrap();
                    } else {
                        if let Some(x) = self.path.node() {
                            assert_eq!(*x, children[offset.to_usize().unwrap() - 1])
                        }
                        self.path.inc(child);
                        assert_eq!(*self.path.offset().unwrap(), offset + 1);
                        self.path.check(self.stores).unwrap_or_else(|_| {
                            panic!(
                                "{:?} {} {:?} {:?} {:?}",
                                node.id(),
                                offset,
                                child,
                                children,
                                self.path
                            )
                        });
                    }
                    self.stack.push((node, offset + 1, Some(children)));
                    let child = if let Some(tid) = self.stores.try_typed(&child) {
                        Id::Query(tid)
                    } else {
                        Id::Other(child)
                    };
                    self.stack.push((child, 0, None));
                    continue;
                } else {
                    self.path.check(self.stores).unwrap();
                    self.path.pop().expect("should not go higher than root");
                    self.path.check(self.stores).unwrap();
                    continue;
                }
            } else {
                let b = match &node {
                    Id::Query(node) => self.stores.resolve_typed(node),
                    Id::Other(node) => {
                        let b = self.stores.node_store().resolve(node);
                        if b.has_children() {
                            let children = b.children().unwrap().iter_children().collect();
                            self.stack.push((Id::Other(*node), 0, Some(children)));
                        }
                        continue;
                    }
                };

                if b.has_children() {
                    self.stack.push((
                        node,
                        0,
                        Some(b.children().unwrap().iter_children().collect()),
                    ));
                }
                return Some(self.path.clone());
            }
        }
    }
}
