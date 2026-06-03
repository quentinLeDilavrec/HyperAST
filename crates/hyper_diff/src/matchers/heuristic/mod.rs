pub mod cd;
pub mod gt;
pub mod lazy_xy_bottom_up_matcher;
pub mod xy_bottom_up_matcher;

mod factorized_bounds {
    use crate::decompressed_tree_store::{
        ContiguousDescendants, DecompressedWithParent, LazyDecompressedTreeStore,
        LazyPOBorrowSlice, PostOrder, PostOrderIterable,
    };

    use hyperast::types::HyperAST;

    pub trait LazyDecompTreeBounds<HAST: HyperAST + Copy, IdS>:
        DecompressedWithParent<HAST, Self::IdD>
        + PostOrder<HAST, IdS>
        + PostOrderIterable<HAST, IdS>
        + LazyDecompressedTreeStore<HAST, IdS>
    {
    }

    impl<HAST, IdS, T> LazyDecompTreeBounds<HAST, IdS> for T
    where
        HAST: HyperAST + Copy,
        T: DecompressedWithParent<HAST, Self::IdD>
            + PostOrder<HAST, IdS>
            + PostOrderIterable<HAST, IdS>
            + LazyDecompressedTreeStore<HAST, IdS>,
    {
    }

    pub trait LazyDecompTreeBorrowBounds<HAST: HyperAST + Copy, IdS>:
        LazyDecompTreeBounds<HAST, IdS> + LazyPOBorrowSlice<HAST, IdS>
    {
    }

    impl<HAST, IdS, T> LazyDecompTreeBorrowBounds<HAST, IdS> for T
    where
        HAST: HyperAST + Copy,
        T: LazyDecompTreeBounds<HAST, IdS> + LazyPOBorrowSlice<HAST, IdS>,
    {
    }

    pub trait DecompTreeBounds<HAST: HyperAST + Copy, IdD>:
        DecompressedWithParent<HAST, IdD>
        + PostOrder<HAST, IdD, IdD = IdD>
        + PostOrderIterable<HAST, IdD>
        + ContiguousDescendants<HAST, IdD>
    {
    }

    impl<HAST, IdD, T> DecompTreeBounds<HAST, IdD> for T
    where
        HAST: HyperAST + Copy,
        T: DecompressedWithParent<HAST, IdD>
            + PostOrder<HAST, IdD, IdD = IdD>
            + PostOrderIterable<HAST, IdD>
            + ContiguousDescendants<HAST, IdD>,
    {
    }
}
