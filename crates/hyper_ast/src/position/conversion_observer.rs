use std::fmt::Debug;

use super::building;
use building::bottom_up;
use building::top_down;

#[derive(PartialEq, Eq, Hash, Clone, Default, Debug)]
#[allow(unused)] // helper for users
pub struct Observed<P>(P);

impl<P> Observed<P> {
    #[allow(unused)] // helper for users
    pub fn into_inner(self) -> P {
        self.0
    }
}

// based of dbg macro
macro_rules! show {
    ($f:path $(,)?) => {
        eprintln!("{:>22}", stringify!($f))
    };
    ($f:path, $val:expr $(,)?) => {
        match $val {
            tmp => {
                eprintln!(
                    "{:>22} = {:?}",
                    format!("{} {}", stringify!($f), stringify!($val)),
                    &&tmp as &dyn std::fmt::Debug,
                );
                tmp
            }
        }
    };
}

impl<P: top_down::CreateBuilder<IdN>, IdN: Debug> top_down::CreateBuilder<IdN> for Observed<P> {
    fn create(root: IdN) -> Self {
        Self(P::create(show!(t_d::create, root)))
    }
}

impl<P: bottom_up::CreateBuilder> bottom_up::CreateBuilder for Observed<P> {
    fn create() -> Self {
        show!(b_u::create);
        Self(P::create())
    }
}

impl<P: top_down::ReceiveDirName<O>, O> top_down::ReceiveDirName<Observed<O>> for Observed<P> {
    fn push(self, dir_name: &str) -> Observed<O> {
        Observed(self.0.push(show!(t_d::receive, dir_name)))
    }
}

impl<P: bottom_up::ReceiveDirName<O>, O> bottom_up::ReceiveDirName<Observed<O>> for Observed<P> {
    fn push(self, dir_name: &str) -> Observed<O> {
        Observed(self.0.push(show!(b_u::receive, dir_name)))
    }
}

impl<P: top_down::SetFileName<O>, O> top_down::SetFileName<Observed<O>> for Observed<P> {
    fn set_file_name(self, file_name: &str) -> Observed<O> {
        Observed(self.0.set_file_name(show!(t_d::set, file_name)))
    }
}

impl<P: top_down::ReceiveOffset<IdO, O>, O, IdO: Debug> top_down::ReceiveOffset<IdO, Observed<O>>
    for Observed<P>
{
    fn push(self, offset: IdO) -> Observed<O> {
        Observed(self.0.push(show!(t_d::receive, offset)))
    }
}

impl<P: bottom_up::ReceiveOffset<IdO, O>, O, IdO: Debug> bottom_up::ReceiveOffset<IdO, Observed<O>>
    for Observed<P>
{
    fn push(self, offset: IdO) -> Observed<O> {
        Observed(self.0.push(show!(b_u::receive, offset)))
    }
}

impl<P: building::SetLen<IdO, O>, O, IdO: Debug> building::SetLen<IdO, Observed<O>>
    for Observed<P>
{
    fn set(self, len: IdO) -> Observed<O> {
        Observed(self.0.set(show!(building::set, len)))
    }
}

impl<P: building::Transition<O>, O> building::Transition<Observed<O>> for Observed<P> {
    fn transit(self) -> Observed<O> {
        show!(building::transit);
        Observed(self.0.transit())
    }
}

impl<P: bottom_up::ReceiveNode<IdN, O>, O, IdN: Debug> bottom_up::ReceiveNode<IdN, Observed<O>>
    for Observed<P>
{
    fn push(self, node: IdN) -> Observed<O> {
        Observed(self.0.push(show!(b_u::receive, node)))
    }
}
impl<P: bottom_up::SetRoot<IdN, O>, O, IdN: Debug> bottom_up::SetRoot<IdN, Observed<O>>
    for Observed<P>
{
    fn set_root(self, root: IdN) -> Observed<O> {
        Observed(self.0.set_root(show!(b_u::set, root)))
    }
}
impl<P: top_down::SetNode<IdN, O>, O, IdN: Debug> top_down::SetNode<IdN, Observed<O>>
    for Observed<P>
{
    fn set_node(self, node: IdN) -> Observed<O> {
        Observed(self.0.set_node(show!(t_d::set, node)))
    }
}
impl<P: top_down::ReceiveIdx<Idx, O>, O, Idx: Debug> top_down::ReceiveIdx<Idx, Observed<O>>
    for Observed<P>
{
    fn push(self, idx: Idx) -> Observed<O> {
        Observed(self.0.push(show!(t_d::receive, idx)))
    }
}
impl<P: top_down::ReceiveIdxNoSpace<Idx, O>, O, Idx: Debug>
    top_down::ReceiveIdxNoSpace<Idx, Observed<O>> for Observed<P>
{
    fn push(self, idx: Idx) -> Observed<O> {
        Observed(self.0.push(show!(t_d::rcv_no_space, idx)))
    }
}
impl<P: bottom_up::ReceiveIdx<Idx, O>, O, Idx: Debug> bottom_up::ReceiveIdx<Idx, Observed<O>>
    for Observed<P>
{
    fn push(self, idx: Idx) -> Observed<O> {
        Observed(self.0.push(show!(b_u::receive, idx)))
    }
}

impl<P: top_down::ReceiveParent<IdN, O>, O, IdN: Debug> top_down::ReceiveParent<IdN, Observed<O>>
    for Observed<P>
{
    fn push(self, parent: IdN) -> Observed<O> {
        Observed(self.0.push(show!(t_d::receive, parent)))
    }
}
impl<P: building::SetLineSpan<T, O>, O, T: Debug> building::SetLineSpan<T, Observed<O>>
    for Observed<P>
{
    fn set(self, lines: T) -> Observed<O> {
        Observed(self.0.set(show!(building::set_span, lines)))
    }
}
impl<P: building::ReceiveRows<T, O>, O, T: Debug> building::ReceiveRows<T, Observed<O>>
    for Observed<P>
{
    fn push(self, row: T) -> Observed<O> {
        Observed(self.0.push(show!(building::receive, row)))
    }
}
impl<P: building::ReceiveColumns<T, O>, O, T: Debug> building::ReceiveColumns<T, Observed<O>>
    for Observed<P>
{
    fn push(self, col: T) -> Observed<O> {
        Observed(self.0.push(show!(building::receive, col)))
    }
}

impl<P: top_down::FileSysReceiver> top_down::FileSysReceiver for Observed<P> {
    type InFile<O> = Observed<P::InFile<O>>;
}

impl<P: bottom_up::FileSysReceiver> bottom_up::FileSysReceiver for Observed<P> {
    type InFile<O> = Observed<P::InFile<O>>;
}
