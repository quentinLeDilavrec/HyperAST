use num_traits::PrimInt;
use std::ops::Range;

use crate::matchers::Mapping;

pub trait MappingStore {
    type Src;
    type Dst;
    fn topit(&mut self, left: usize, right: usize);
    fn len(&self) -> usize;
    fn is_empty(&self) -> bool {
        self.len() == 0
    }
    fn capacity(&self) -> (usize, usize);
    fn has(&self, src: &Self::Src, dst: &Self::Dst) -> bool;
    fn link(&mut self, src: Self::Src, dst: Self::Dst);
    fn cut(&mut self, src: Self::Src, dst: Self::Dst);
    fn is_src(&self, src: &Self::Src) -> bool;
    fn is_dst(&self, dst: &Self::Dst) -> bool;
}

pub trait MonoMappingStore: MappingStore {
    type Iter<'a>: Iterator<Item = (Self::Src, Self::Dst)>
    where
        Self: 'a;
    fn get_src_unchecked(&self, dst: &Self::Dst) -> Self::Src;
    fn get_dst_unchecked(&self, src: &Self::Src) -> Self::Dst;
    fn get_src(&self, dst: &Self::Dst) -> Option<Self::Src>;
    fn get_dst(&self, src: &Self::Src) -> Option<Self::Dst>;
    fn link_if_both_unmapped(&mut self, t1: Self::Src, t2: Self::Dst) -> bool;
    fn iter(&self) -> Self::Iter<'_>;
    fn number_of_common_descendants_ranges(
        &self,
        src: &std::ops::Range<Self::Src>,
        dst: &std::ops::Range<Self::Dst>,
    ) -> u32
    where
        Self::Src: num_traits::PrimInt,
        Self::Dst: num_traits::PrimInt,
        Self: Sized,
    {
        crate::similarity_metrics::number_of_common_descendants_ranges(src, dst, self)
    }
}

pub trait MultiMappingStore: MappingStore {
    type Iter1<'a>: Iterator<Item = Self::Src>
    where
        Self: 'a;
    type Iter2<'a>: Iterator<Item = Self::Dst>
    where
        Self: 'a;
    fn get_srcs(&self, dst: &Self::Dst) -> &[Self::Src];
    fn get_dsts(&self, src: &Self::Src) -> &[Self::Dst];
    fn all_mapped_srcs(&self) -> Self::Iter1<'_>;
    fn all_mapped_dsts(&self) -> Self::Iter2<'_>;
    fn is_src_unique(&self, src: &Self::Src) -> bool;
    fn is_dst_unique(&self, dst: &Self::Dst) -> bool;
}

pub trait MultiRangeMappingStore<Src, Dst>: MappingStore {
    fn get_src_ranges(&self, dst: &Self::Dst) -> impl Iterator<Item = Range<Src>>
    where
        Self::Src: PrimInt,
        Self::Dst: PrimInt;
    fn get_dst_ranges(&self, src: &Self::Src) -> impl Iterator<Item = Range<Dst>>
    where
        Self::Src: PrimInt,
        Self::Dst: PrimInt;
}

impl<Dsrc, Ddst, M: MappingStore> MappingStore for crate::matchers::Mapping<Dsrc, Ddst, M> {
    type Src = M::Src;

    type Dst = M::Dst;

    fn topit(&mut self, _left: usize, _right: usize) {
        todo!()
    }

    fn len(&self) -> usize {
        todo!()
    }

    fn capacity(&self) -> (usize, usize) {
        todo!()
    }

    fn has(&self, _src: &Self::Src, _dst: &Self::Dst) -> bool {
        todo!()
    }

    fn link(&mut self, _src: Self::Src, _dst: Self::Dst) {
        todo!()
    }

    fn cut(&mut self, _src: Self::Src, _dst: Self::Dst) {
        todo!()
    }

    fn is_src(&self, src: &Self::Src) -> bool {
        self.mappings.is_src(src)
    }

    fn is_dst(&self, dst: &Self::Dst) -> bool {
        self.mappings.is_dst(dst)
    }
}

impl<Dsrc, Ddst, M: MultiMappingStore, Src, Dst> MultiRangeMappingStore<Src, Dst>
    for Mapping<Dsrc, Ddst, M>
where
    Ddst: crate::decompressed_tree_store::RawContiguousDescendants<Self::Dst, Dst>,
    Dsrc: crate::decompressed_tree_store::RawContiguousDescendants<Self::Src, Src>,
{
    fn get_src_ranges(&self, dst: &Self::Dst) -> impl Iterator<Item = Range<Src>>
    where
        Self::Src: PrimInt,
        Self::Dst: PrimInt,
    {
        self.mappings
            .get_srcs(dst)
            .iter()
            .map(|x| self.src_arena.range(x))
    }
    fn get_dst_ranges(&self, src: &Self::Src) -> impl Iterator<Item = Range<Dst>>
    where
        Self::Src: PrimInt,
        Self::Dst: PrimInt,
    {
        self.mappings
            .get_dsts(src)
            .iter()
            .map(|x| self.dst_arena.range(x))
    }
}
