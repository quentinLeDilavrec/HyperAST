use std::fmt::{Debug, Display};
use std::path::PathBuf;

use crate::PrimInt;

#[derive(PartialEq, Eq, Hash, Clone, Default)]
pub struct Position<F, T> {
    file: F,
    offset: T,
    len: T,
}

impl<F, T: PrimInt> Position<F, T> {
    pub fn new(file: F, offset: T, len: T) -> Self {
        Self { file, offset, len }
    }
    pub fn inc_offset(&mut self, x: T) {
        self.offset += x;
    }
    pub fn set_len(&mut self, x: T) {
        self.len = x;
    }
    pub fn range(&self) -> std::ops::Range<T> {
        self.offset..(self.offset + self.len)
    }
}
impl<F: Eq, T: PrimInt> Position<F, T> {
    pub fn try_merge(&mut self, other: Position<F, T>) {
        if self.file != other.file {
            log::warn!("trying to merge positions from different files");
            return;
        }
        self.offset = self.offset.min(other.offset);
        let end = self.offset + self.len;
        let end = end.max(other.offset + other.len);
        self.len = end - self.offset;
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
            .field("offset", &self.offset)
            .field("len", &self.len)
            .finish()
    }
}

impl<T: PrimInt + Display> Display for Position<PathBuf, T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{{\"offset\":{},\"len\":{},\"file\":{:?}}}",
            &self.offset, &self.len, &self.file
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
            offset: range.start,
            len,
        }
    }
}

use super::spaces_related::{NoSpacePrepareParams, SealedFileTopDownPosBuilder, TopDownPosBuilder};

impl<IdN, Idx: PrimInt, IdO: PrimInt + Default>
    TopDownPosBuilder<IdN, Idx, IdO, NoSpacePrepareParams<Idx>> for Position<PathBuf, IdO>
{
    type Prepared = Position<PathBuf, IdO>;

    type SealedFile = Position<PathBuf, IdO>;

    fn seal_path(mut self, file_name: &str) -> Self::SealedFile {
        self.file.push(file_name);
        self
    }

    fn seal_without_path(self) -> Self::SealedFile {
        self
    }

    fn push(&mut self, _parent: IdN, _offset: Idx, dir_name: &str, _additional: ()) {
        self.file.push(dir_name);
    }

    fn finish(self, _node: IdN) -> Self::Prepared {
        todo!("how exactly should directories be handled")
    }
}
impl<IdN, Idx: PrimInt, IdO: PrimInt>
    SealedFileTopDownPosBuilder<IdN, Idx, IdO, NoSpacePrepareParams<Idx>>
    for Position<PathBuf, IdO>
{
    type Prepared = Position<PathBuf, IdO>;

    fn push(&mut self, _parent: IdN, _idx: Idx, offset: IdO, (_no_s_idx,): (Idx,)) {
        self.offset += offset;
    }

    fn finish(self, _node: IdN, len: Idx, _additional: ()) -> Self::Prepared {
        assert_eq!(self.len, num::zero());
        let len = num::cast(len).unwrap();
        Self::Prepared {
            file: self.file,
            offset: self.offset,
            len,
        }
    }
}

impl<P, IdO> super::node_filter_traits::Full for Position<P, IdO> {}

mod impl_receivers {
    use super::super::building;
    use crate::PrimInt;
    use building::bottom_up;
    use building::top_down;
    use std::path::PathBuf;

    impl<IdO: PrimInt, IdN> top_down::CreateBuilder<IdN> for super::Position<PathBuf, IdO> {
        fn create(_root: IdN) -> Self {
            Self {
                file: Default::default(),
                offset: num::zero(),
                len: num::zero(),
            }
        }
    }

    impl<IdO: PrimInt> bottom_up::CreateBuilder for super::Position<PathBuf, IdO> {
        fn create() -> Self {
            Self {
                file: Default::default(),
                offset: num::zero(),
                len: num::zero(),
            }
        }
    }

    impl<IdO: PrimInt> top_down::ReceiveDirName<Self> for super::Position<PathBuf, IdO> {
        fn push(mut self, dir_name: &str) -> Self {
            self.file.push(dir_name);
            self
        }
    }

    impl<IdO: PrimInt> bottom_up::ReceiveDirName<Self> for super::Position<PathBuf, IdO> {
        fn push(mut self, dir_name: &str) -> Self {
            self.file = std::path::PathBuf::from(dir_name).join(self.file);
            self
        }
    }

    impl<IdO: PrimInt> top_down::SetFileName<Self> for super::Position<PathBuf, IdO> {
        fn set_file_name(mut self, file_name: &str) -> Self {
            self.file.push(file_name);
            self
        }
    }

    impl<IdO: PrimInt> top_down::ReceiveOffset<IdO, Self> for super::Position<PathBuf, IdO> {
        fn push(mut self, offset: IdO) -> Self {
            self.offset += offset;
            self
        }
    }

    impl<IdO: PrimInt> bottom_up::ReceiveOffset<IdO, Self> for super::Position<PathBuf, IdO> {
        fn push(mut self, offset: IdO) -> Self {
            self.offset += offset;
            self
        }
    }

    impl<IdO: PrimInt> building::SetLen<IdO, Self> for super::Position<PathBuf, IdO> {
        fn set(mut self, len: IdO) -> Self {
            self.len = len;
            self
        }
    }

    building::default_impl_receivers! {
        impl<P, IdO>
        building::Transition<Self>
        <IdN> bottom_up::ReceiveNode<IdN, Self>
        <IdN> bottom_up::SetRoot<IdN, Self>
        <IdN> top_down::SetNode<IdN, Self>
        <Idx> top_down::ReceiveIdx<Idx, Self>
        <Idx> top_down::ReceiveIdxNoSpace<Idx, Self>
        <Idx> bottom_up::ReceiveIdx<Idx, Self>
        <IdN> top_down::ReceiveParent<IdN, Self>
        <T> building::SetLineSpan<T, Self>
        <T> building::ReceiveRows<T, Self>
        <T> building::ReceiveColumns<T, Self>
        top_down::FileSysReceiver
        bottom_up::FileSysReceiver
        for super::Position<P, IdO>
    }
}
