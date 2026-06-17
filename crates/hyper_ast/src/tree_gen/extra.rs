use super::*;

impl<Acc, Extra> WithExtra for AccWithExtra<Acc, Extra> {
    type Extra = Extra;

    fn extra(&mut self) -> &mut Self::Extra {
        &mut self.1
    }
}

impl<Acc, Extra> std::ops::Deref for AccWithExtra<Acc, Extra> {
    type Target = Acc;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl<Acc, Extra> std::ops::DerefMut for AccWithExtra<Acc, Extra> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.0
    }
}

impl<Acc, Extra: Default> From<Acc> for AccWithExtra<Acc, Extra> {
    fn from(acc: Acc) -> Self {
        AccWithExtra(acc, Extra::default())
    }
}

impl<Acc, Extra, Id: Clone> WithChildren<Id> for AccWithExtra<Acc, Extra>
where
    Acc: WithChildren<Id>,
{
    fn children(&self) -> &[Id] {
        self.0.children()
    }
}

impl<Acc: types::Typed, Extra> types::Typed for AccWithExtra<Acc, Extra> {
    type Type = <Acc as types::Typed>::Type;
    fn get_type(&self) -> Self::Type {
        self.0.get_type()
    }
}

impl<Acc: AccIndentation, Extra: std::ops::AddAssign> AccIndentation for AccWithExtra<Acc, Extra> {
    fn indentation(&self) -> &Spaces {
        self.0.indentation()
    }
}

impl<Acc: Accumulator, Extra: std::ops::AddAssign> Accumulator for AccWithExtra<Acc, Extra> {
    type Node = NodeWithExtra<<Acc as Accumulator>::Node, Extra>;
    fn push(&mut self, full_node: Self::Node) {
        self.0.push(full_node.node);
        self.1 += full_node.extra;
    }
}

pub struct NodeWithExtra<N, Extra> {
    pub node: N,
    pub extra: Extra,
}

impl<N, Extra> std::ops::Deref for NodeWithExtra<N, Extra> {
    type Target = N;

    fn deref(&self) -> &Self::Target {
        &self.node
    }
}

impl<N, Extra> From<(N, Extra)> for NodeWithExtra<N, Extra> {
    fn from((node, extra): (N, Extra)) -> Self {
        NodeWithExtra { node, extra }
    }
}

impl<N, Extra: Default> From<N> for NodeWithExtra<N, Extra> {
    fn from(node: N) -> Self {
        NodeWithExtra {
            node,
            extra: Default::default(),
        }
    }
}

impl<Acc: WithByteRange, Extra> WithByteRange for AccWithExtra<Acc, Extra> {
    fn has_children(&self) -> bool {
        self.0.has_children()
    }

    fn begin_byte(&self) -> usize {
        self.0.begin_byte()
    }

    fn end_byte(&self) -> usize {
        self.0.end_byte()
    }
}

impl<Acc: WithRole<R>, Extra, R> WithRole<R> for AccWithExtra<Acc, Extra> {
    fn role_at(&self, idx: usize) -> Option<R> {
        self.0.role_at(idx)
    }

    fn role(&self) -> Option<R> {
        self.0.role()
    }
}

// TODO implement all the acc interfaces on AccWithExtra
