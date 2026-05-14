use num_traits::{PrimInt, cast};

use crate::mappings::MultiMappingStore;

use super::MappingStore;

pub struct MultiVecStore<T> {
    pub src_to_dsts: Vec<Option<Vec<T>>>,
    pub dst_to_srcs: Vec<Option<Vec<T>>>,
}

impl<T: PrimInt> Clone for MultiVecStore<T> {
    fn clone(&self) -> Self {
        Self {
            src_to_dsts: self.src_to_dsts.clone(),
            dst_to_srcs: self.dst_to_srcs.clone(),
        }
    }
}

impl<T> Default for MultiVecStore<T> {
    fn default() -> Self {
        Self {
            src_to_dsts: Default::default(),
            dst_to_srcs: Default::default(),
        }
    }
}

impl<T: PrimInt> MappingStore for MultiVecStore<T> {
    type Src = T;
    type Dst = T;

    fn len(&self) -> usize {
        self.src_to_dsts
            .iter()
            .filter_map(|x| x.as_ref())
            .map(|x| x.len())
            .sum()
    }

    fn capacity(&self) -> (usize, usize) {
        (self.src_to_dsts.len(), self.dst_to_srcs.len())
    }

    fn link(&mut self, src: T, dst: T) {
        // self.src_to_dsts[src.to_usize().unwrap()].get_or_insert_default().push(dst); // todo when not unstable feature
        if self.src_to_dsts[src.to_usize().unwrap()].is_none() {
            self.src_to_dsts[src.to_usize().unwrap()] = Some(vec![])
        }
        self.src_to_dsts[src.to_usize().unwrap()]
            .as_mut()
            .unwrap()
            .push(dst);
        if self.dst_to_srcs[dst.to_usize().unwrap()].is_none() {
            self.dst_to_srcs[dst.to_usize().unwrap()] = Some(vec![])
        }
        self.dst_to_srcs[dst.to_usize().unwrap()]
            .as_mut()
            .unwrap()
            .push(src);
    }

    fn cut(&mut self, src: T, dst: T) {
        if let Some(i) = self.src_to_dsts[src.to_usize().unwrap()]
            .as_ref()
            .and_then(|v| v.iter().position(|x| x == &dst))
        {
            if self.src_to_dsts[src.to_usize().unwrap()]
                .as_ref()
                .unwrap()
                .len()
                == 1
            {
                self.src_to_dsts[src.to_usize().unwrap()] = None;
            } else {
                self.src_to_dsts[src.to_usize().unwrap()]
                    .as_mut()
                    .unwrap()
                    .remove(i);
            }
        }
        if let Some(i) = self.dst_to_srcs[dst.to_usize().unwrap()]
            .as_ref()
            .and_then(|v| v.iter().position(|x| x == &src))
        {
            if self.dst_to_srcs[dst.to_usize().unwrap()]
                .as_ref()
                .unwrap()
                .len()
                == 1
            {
                self.dst_to_srcs[dst.to_usize().unwrap()] = None;
            } else {
                self.dst_to_srcs[dst.to_usize().unwrap()]
                    .as_mut()
                    .unwrap()
                    .remove(i);
            }
        }
    }

    fn is_src(&self, src: &T) -> bool {
        if src.to_usize().unwrap() >= self.src_to_dsts.len() {
            return false;
        }
        self.src_to_dsts[src.to_usize().unwrap()].is_some()
    }

    fn is_dst(&self, dst: &T) -> bool {
        if dst.to_usize().unwrap() >= self.dst_to_srcs.len() {
            return false;
        }
        self.dst_to_srcs[dst.to_usize().unwrap()].is_some()
    }

    fn topit(&mut self, left: usize, right: usize) {
        self.src_to_dsts.resize(left, None);
        self.dst_to_srcs.resize(right, None);
    }

    fn has(&self, src: &Self::Src, dst: &Self::Dst) -> bool {
        self.src_to_dsts[src.to_usize().unwrap()]
            .as_ref()
            .map(|v| v.contains(dst))
            .unwrap_or(false)
            && self.dst_to_srcs[dst.to_usize().unwrap()]
                .as_ref()
                .map(|v| v.contains(src))
                .unwrap_or(false)
    }
}

impl<T: PrimInt> MultiMappingStore for MultiVecStore<T> {
    type Iter1<'a>
        = Iter<'a, T>
    where
        T: 'a;
    type Iter2<'a>
        = Iter<'a, T>
    where
        T: 'a;
    fn get_srcs(&self, dst: &Self::Dst) -> &[Self::Src] {
        self.dst_to_srcs[cast::<_, usize>(*dst).unwrap()]
            .as_deref()
            .unwrap_or(&[])
    }

    fn get_dsts(&self, src: &Self::Src) -> &[Self::Dst] {
        self.src_to_dsts[cast::<_, usize>(*src).unwrap()]
            .as_deref()
            .unwrap_or(&[])
    }

    fn all_mapped_srcs(&self) -> Iter<'_, Self::Src> {
        Iter {
            v: self.src_to_dsts.iter().enumerate(),
        }
    }

    fn all_mapped_dsts(&self) -> Iter<'_, Self::Dst> {
        Iter {
            v: self.dst_to_srcs.iter().enumerate(),
        }
    }

    fn is_src_unique(&self, src: &Self::Src) -> bool {
        self.get_dsts(src).len() == 1
    }

    fn is_dst_unique(&self, dst: &Self::Dst) -> bool {
        self.get_srcs(dst).len() == 1
    }
}

pub struct Iter<'a, T: 'a> {
    v: std::iter::Enumerate<core::slice::Iter<'a, Option<Vec<T>>>>,
}

impl<T: PrimInt> Iterator for Iter<'_, T> {
    type Item = T;

    fn next(&mut self) -> Option<Self::Item> {
        let mut a = self.v.next();
        loop {
            if let Some((i, x)) = a {
                if x.is_some() {
                    return Some(cast::<_, T>(i).unwrap());
                } else {
                    a = self.v.next();
                }
            } else {
                return None;
            }
        }
    }
}
