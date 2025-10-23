pub mod cd;
pub mod gt;
pub mod xy_bottom_up_matcher;

mod factorized_bounds {
    use crate::decompressed_tree_store::{
        ContiguousDescendants, DecompressedWithParent, LazyDecompressedTreeStore,
        LazyPOBorrowSlice, PostOrder, PostOrderIterable,
    };

    use hyperast::types::HyperAST;

    pub trait LazyDecompTreeBounds<HAST: HyperAST + Copy, IdS>:
        DecompressedWithParent<HAST, Self::IdD>
        + PostOrder<HAST, Self::IdD, IdS>
        + PostOrderIterable<HAST, Self::IdD, IdS>
        + LazyDecompressedTreeStore<HAST, IdS>
    {
    }

    impl<HAST, IdS, T> LazyDecompTreeBounds<HAST, IdS> for T
    where
        HAST: HyperAST + Copy,
        T: DecompressedWithParent<HAST, Self::IdD>
            + PostOrder<HAST, Self::IdD, IdS>
            + PostOrderIterable<HAST, Self::IdD, IdS>
            + LazyDecompressedTreeStore<HAST, IdS>,
    {
    }

    pub trait LazyDecompTreeBorrowBounds<HAST: HyperAST + Copy, IdS>:
        LazyDecompTreeBounds<HAST, IdS> + LazyPOBorrowSlice<HAST, Self::IdD, IdS>
    {
    }

    impl<HAST, IdS, T> LazyDecompTreeBorrowBounds<HAST, IdS> for T
    where
        HAST: HyperAST + Copy,
        T: LazyDecompTreeBounds<HAST, IdS> + LazyPOBorrowSlice<HAST, Self::IdD, IdS>,
    {
    }

    pub trait DecompTreeBounds<HAST: HyperAST + Copy, IdD>:
        DecompressedWithParent<HAST, IdD>
        + PostOrder<HAST, IdD>
        + PostOrderIterable<HAST, IdD>
        + ContiguousDescendants<HAST, IdD>
    {
    }

    impl<HAST, IdD, T> DecompTreeBounds<HAST, IdD> for T
    where
        HAST: HyperAST + Copy,
        T: DecompressedWithParent<HAST, IdD>
            + PostOrder<HAST, IdD>
            + PostOrderIterable<HAST, IdD>
            + ContiguousDescendants<HAST, IdD>,
    {
    }
}
