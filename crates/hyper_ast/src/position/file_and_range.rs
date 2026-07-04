use crate::PrimInt;
use std::fmt::{Debug, Display};
use std::path::PathBuf;

#[derive(PartialEq, Eq, Hash, Clone, Default)]
pub struct Position<F, T> {
    file: F,
    start: T,
    len: T,
}

impl<F, T: PrimInt> Position<F, T> {
    pub fn new(file: F, start: T, len: T) -> Self {
        Self { file, start, len }
    }
    pub fn range(&self) -> std::ops::Range<T> {
        self.start..(self.start + self.len)
    }
}

impl<F: std::ops::Deref, T: PrimInt> Position<F, T> {
    pub fn file(&self) -> &F::Target {
        self.file.deref()
    }
}

impl<T: PrimInt> Position<PathBuf, T> {
    pub fn inc_path(&mut self, s: &str) {
        self.file.push(s);
    }
}

impl<F: Debug, T: PrimInt> Debug for Position<F, T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("Position")
            .field("file", &self.file)
            .field("start", &self.start)
            .field("len", &self.len)
            .finish()
    }
}

impl<T: PrimInt + Display> Display for Position<PathBuf, T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{{\"start\":{},\"len\":{},\"file\":{:?}}}",
            &self.start, &self.len, &self.file
        )
    }
}

// TODO use an interface for TopDownPositionBuilder, should actually be the same as position here, this way you can see the generated pos as a DTO
// TODO in the same way finishing a prepare struct could directly be converted into a position, or be an accumulator itself (actually better for some structs)
impl<IdN, Idx, IdO: PrimInt> From<super::spaces_related::TopDownPositionBuilder<IdN, Idx, IdO>>
    for Position<PathBuf, IdO>
{
    fn from(val: super::spaces_related::TopDownPositionBuilder<IdN, Idx, IdO>) -> Self {
        // TODO how to handle position of directory ?
        let range = val.range.unwrap();
        let len = range.end - range.start;
        Position {
            file: val.file,
            start: range.start,
            len,
        }
    }
}

mod impl_receivers {
    use super::super::building;
    use crate::PrimInt;
    use building::bottom_up;
    use building::top_down;
    use std::path::PathBuf;

    impl<T: PrimInt> top_down::CreateBuilder for super::Position<PathBuf, T> {
        fn create() -> Self {
            Self {
                file: Default::default(),
                start: num::zero(),
                len: num::zero(),
            }
        }
    }

    impl<T: PrimInt> bottom_up::CreateBuilder for super::Position<PathBuf, T> {
        fn create() -> Self {
            Self {
                file: Default::default(),
                start: num::zero(),
                len: num::zero(),
            }
        }
    }

    impl<T: PrimInt> building::ReceiveRows<T, Self> for super::Position<PathBuf, T> {
        fn push(mut self, row: T) -> Self {
            self.start += row;
            self
        }
    }

    impl<T: PrimInt> building::ReceiveColumns<T, Self> for super::Position<PathBuf, T> {
        fn push(self, _col: T) -> Self {
            self
        }
    }

    impl<T: PrimInt> building::SetLineSpan<T, Self> for super::Position<PathBuf, T> {
        fn set(mut self, lines: T) -> Self {
            self.len = lines;
            self
        }
    }

    building::default_impl_receivers! {
        impl<T>
        building::Transition<Self>
        <IdN> bottom_up::ReceiveNode<IdN, Self>
        <IdN> bottom_up::SetRoot<IdN, Self>
        <IdO> top_down::ReceiveOffset<IdO, Self>
        top_down::ReceiveDirName<Self>
        bottom_up::ReceiveDirName<Self>
        <IdN> top_down::SetNode<IdN, Self>
        <IdO> building::SetLen<IdO, Self>
        top_down::SetFileName<Self>
        <Idx> top_down::ReceiveIdx<Idx, Self>
        <Idx> top_down::ReceiveIdxNoSpace<Idx, Self>
        <Idx> bottom_up::ReceiveIdx<Idx, Self>
        <IdO> bottom_up::ReceiveOffset<IdO, Self>
        <IdN> top_down::ReceiveParent<IdN, Self>
        top_down::FileSysReceiver
        for super::Position<PathBuf, T>
    }
}
