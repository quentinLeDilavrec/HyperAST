use std::fmt::{Debug, Display};

use crate::PrimInt;

#[derive(PartialEq, Eq, Hash, Clone, Default)]
pub struct RowCol<T> {
    row: T,
    col: T,
}

impl<T: PrimInt> RowCol<T> {
    pub fn new(row: T, col: T) -> Self {
        Self { row, col }
    }
    pub fn inc_row(&mut self, x: T) {
        self.row += x;
    }
    pub fn inc_col(&mut self, x: T) {
        self.col += x;
    }
    pub fn row(&self) -> T {
        self.row
    }
    pub fn col(&self) -> T {
        self.col
    }
}

impl<T: PrimInt> Debug for RowCol<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("RowCol")
            .field("row", &self.row)
            .field("col", &self.col)
            .finish()
    }
}

impl<T: PrimInt + Display> Display for RowCol<T> {
    fn fmt(&self, _f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        todo!()
    }
}

mod impl_receivers {
    use super::super::building;
    use crate::PrimInt;
    use building::bottom_up;
    use building::top_down;

    impl<T: PrimInt, IdN> top_down::CreateBuilder<IdN> for super::RowCol<T> {
        fn create(_root: IdN) -> Self {
            Self {
                row: num::zero(),
                col: num::zero(),
            }
        }
    }

    impl<T: PrimInt> bottom_up::CreateBuilder for super::RowCol<T> {
        fn create() -> Self {
            Self {
                row: num::zero(),
                col: num::zero(),
            }
        }
    }

    impl<T: PrimInt> building::ReceiveRows<T, Self> for super::RowCol<T> {
        fn push(mut self, row: T) -> Self {
            self.row += row;
            self
        }
    }

    impl<T: PrimInt> building::ReceiveColumns<T, Self> for super::RowCol<T> {
        fn push(mut self, col: T) -> Self {
            self.col += col;
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
        building::SetLineSpan<T, Self>
        <IdN> top_down::ReceiveParent<IdN, Self>
        top_down::FileSysReceiver
        bottom_up::FileSysReceiver
        for super::RowCol<T>
    }

    // impl<IdN, Idx, T: PrimInt> top_down::ReceiveInFile<IdN, Idx, Self> for super::Position<PathBuf, IdO> {
    //     type S1 = Self;
    //     type S2 = Self;

    //     fn finish(self) -> Self {
    //         self
    //     }
    // }
    // impl<IdN, Idx, T: PrimInt> top_down::ReceiveDir<IdN, Idx, Self> for super::Position<PathBuf, IdO> {
    //     type SA1 = Self;

    //     type SA2 = Self;

    //     type SB1 = Self;

    //     fn go_inside_file(mut self, file_name: &str) -> Self::SB1 {
    //         self.file.push(file_name);
    //         self
    //     }

    //     fn finish(self) -> Self {
    //         self
    //     }
    // }
}
