use std::fmt::Debug;
use std::ops::AddAssign;

use hyperast::PrimInt as _;
use hyperast::position::StructuralPosition;
use hyperast::position::{TreePath, TreePathMut};
use hyperast::store::defaults::NodeIdentifier;
use hyperast::types::{Childrn as _, Tree as _, WithChildren as _};
use hyperast::types::{LabelStore as _, Labeled as _, Typed as _};
use hyperast_gen_ts_xml::Type;

use super::SimpleStores;

type XmlIdN = hyperast_gen_ts_xml::TIdN<NodeIdentifier>;
type XmlNode<'a> = hyperast::store::nodes::legion::HashedNodeRef<'a, XmlIdN>;

pub struct IterMavenModules2<'a> {
    pub(crate) stores: &'a SimpleStores,
    pub(crate) parents: Vec<NodeIdentifier>,
    pub(crate) offsets: Vec<u16>,
    /// to tell that we need to pop a parent, we could also use a bitvec instead of Option::None
    pub(crate) remaining: Vec<Option<NodeIdentifier>>,
}

impl<'a> Debug for IterMavenModules2<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("IterMavenModules")
            .field("parents", &self.parents())
            .field("offsets", &self.offsets())
            .field("remaining", &self.remaining)
            .finish()
    }
}

impl<'a> Iterator for IterMavenModules2<'a> {
    type Item = StructuralPosition;

    fn next(&mut self) -> Option<Self::Item> {
        self.next_node()
            .map(|x| (self.parents().to_vec(), self.offsets().to_vec(), x).into())
    }
}

impl<'a> IterMavenModules2<'a> {
    pub fn new(stores: &'a SimpleStores, root: NodeIdentifier) -> Self {
        Self {
            stores,
            parents: vec![],
            offsets: vec![0],
            remaining: vec![Some(root)],
        }
    }
    pub fn parents(&self) -> &[NodeIdentifier] {
        &self.parents[..self.parents.len() - 1]
    }
    pub fn offsets(&self) -> &[u16] {
        &self.offsets[..self.offsets.len() - 1]
    }

    pub(crate) fn next_node(&mut self) -> Option<NodeIdentifier> {
        let x;
        loop {
            if let Some(c) = self.remaining.pop()? {
                self.offsets.last_mut().unwrap().add_assign(1);
                x = c;
                break;
            }
            self.offsets.pop();
            self.parents.pop();
        }

        let b = (self.stores.node_store)
            .try_resolve_typed::<XmlIdN>(&x)
            .unwrap()
            .0;
        let t = b.get_type();

        let is_src = if b.has_label() {
            self.stores
                .label_store
                .resolve(b.get_label_unchecked())
                .eq("src")
        } else {
            false
        };

        if is_src {
            return self.next_node();
        } else if t != Type::MavenDirectory {
            return self.next_node();
        }

        self.parents.push(x);
        self.offsets.push(0);
        self.remaining.push(None);
        if b.has_children() {
            let cs = b.children().unwrap().iter_children();
            self.remaining.extend(cs.rev().map(|x| Some(x)));
        }

        let contains_pom = b.children().unwrap_or_default().iter_children().any(|x| {
            let Some(n) = self.stores.node_store.try_resolve_typed::<XmlIdN>(&x) else {
                return false;
            };
            let n = n.0;
            log::debug!("f {:?}", n.get_type());
            if !n.get_type().eq(&Type::Document) {
                return false;
            }
            if !n.has_label() {
                return false;
            }
            log::debug!(
                "f name: {:?}",
                self.stores.label_store.resolve(n.get_label_unchecked())
            );
            self.stores
                .label_store
                .resolve(n.get_label_unchecked())
                .eq("pom.xml")
        });

        if contains_pom {
            Some(x)
        } else {
            while !self.remaining.is_empty() {
                if let Some(x) = self.next_node() {
                    return Some(x);
                }
            }
            None
        }
    }
}

pub struct IterMavenModules<'a, T: TreePath<NodeIdentifier>> {
    stores: &'a SimpleStores,
    path: T,
    stack: Vec<(NodeIdentifier, u16, Option<Vec<NodeIdentifier>>)>,
}

impl<'a, T: TreePathMut<NodeIdentifier, u16> + Debug + Clone> Iterator for IterMavenModules<'a, T> {
    type Item = T;

    fn next(&mut self) -> Option<Self::Item> {
        let (node, offset, children) = self.stack.pop()?;
        let Some(children) = children else {
            return self.when_no_children(node);
        };
        if offset.index() >= children.len() {
            self.path.check(self.stores).unwrap();
            self.path.pop().expect("should not go higher than root");
            self.path.check(self.stores).unwrap();
            return self.next();
        }
        let child = children[offset.index()];
        self.path.check(self.stores).unwrap();
        {
            let b = self
                .stores
                .node_store
                .try_resolve_typed::<XmlIdN>(&node)
                .unwrap()
                .0;
            if b.has_children() {
                let len = b.child_count();
                let cs = b.children().unwrap();
                // println!("children: {:?} {} {:?}", node,cs.len(),cs);
                assert!(offset < len);
                assert_eq!(child, cs[offset]);
            } else {
                panic!()
            }
        }
        if offset == 0 {
            if let Some(x) = self.path.node() {
                assert_eq!(*x, node)
            }
            self.path.goto(child, offset);
            self.path.check(self.stores).unwrap();
        } else {
            if let Some(x) = self.path.node() {
                assert_eq!(*x, children[offset.index() - 1])
            }
            self.path.inc(child);
            assert_eq!(*self.path.offset().unwrap(), offset + 1);
            self.path.check(self.stores).unwrap_or_else(|_| {
                panic!(
                    "{:?} {} {:?} {:?} {:?}",
                    node, offset, child, children, self.path
                )
            });
        }
        self.stack.push((node, offset + 1, Some(children)));
        self.stack.push((child, 0, None));
        return self.next();
    }
}

impl<'a, T: TreePathMut<NodeIdentifier, u16> + Debug + Clone> IterMavenModules<'a, T> {
    fn when_no_children(&mut self, node: NodeIdentifier) -> Option<T> {
        let b = self
            .stores
            .node_store
            .try_resolve_typed::<XmlIdN>(&node)
            .unwrap()
            .0;
        if self.is_dead_end(&b) {
            return self.next();
        }
        if b.has_children() {
            let children = b.children();
            self.stack
                .push((node, 0, Some(children.unwrap().iter_children().collect())));
        }
        if self.is_matching(&b) {
            self.path.check(self.stores).unwrap();
            return Some(self.path.clone());
        }
        return self.next();
    }
}

impl<'a, T: TreePath<NodeIdentifier>> IterMavenModules<'a, T> {
    pub fn new(stores: &'a SimpleStores, path: T, root: NodeIdentifier) -> Self {
        let stack = vec![(root, 0, None)];
        Self {
            stores,
            path,
            stack,
        }
    }

    fn is_dead_end(&self, b: &XmlNode<'a>) -> bool {
        let t = b.get_type();
        let is_src = if b.has_label() {
            self.stores
                .label_store
                .resolve(b.get_label_unchecked())
                .eq("src")
        } else {
            false
        };

        is_src || t != Type::MavenDirectory
    }

    fn is_matching(&self, b: &XmlNode<'a>) -> bool {
        b.children().unwrap().iter_children().any(|x| {
            let Some(n) = self.stores.node_store.try_resolve_typed::<XmlIdN>(&x) else {
                return false;
            };
            let n = n.0;
            log::debug!("f {:?}", n.get_type());
            if !n.get_type().eq(&Type::Document) {
                return false;
            }
            if !n.has_label() {
                return false;
            }
            log::debug!(
                "f name: {:?}",
                self.stores.label_store.resolve(n.get_label_unchecked())
            );
            self.stores
                .label_store
                .resolve(n.get_label_unchecked())
                .eq("pom.xml")
        })
    }
}
