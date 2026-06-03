#![allow(unused)]
use num_traits::PrimInt;
use std::fmt::Debug;
use std::hash::Hash;

use hyperast::compat::HashMap;

use super::{MappingStore, MonoMappingStore};

#[derive(Debug)]
pub struct HashStore<T> {
    pub src_to_dst: HashMap<T, T>,
    pub dst_to_src: HashMap<T, T>,
}

impl<T> Default for HashStore<T> {
    fn default() -> Self {
        Self {
            src_to_dst: Default::default(),
            dst_to_src: Default::default(),
        }
    }
}

impl<T: PrimInt + Debug + Hash> HashStore<T> {
    pub fn iter(&self) -> impl Iterator<Item = (T, T)> + '_ {
        self.src_to_dst.iter().map(|(src, dst)| (*src, *dst))
    }

    pub fn link_if_both_unmapped(&mut self, t1: T, t2: T) -> bool {
        if self.is_src(&t1) && self.is_dst(&t2) {
            self.link(t1, t2);
            true
        } else {
            false
        }
    }
}

impl<T: PrimInt + Debug> Clone for HashStore<T> {
    fn clone(&self) -> Self {
        Self {
            src_to_dst: self.src_to_dst.clone(),
            dst_to_src: self.dst_to_src.clone(),
        }
    }
}

impl<T: PrimInt + Debug + Hash> MappingStore for HashStore<T> {
    type Src = T;
    type Dst = T;

    fn len(&self) -> usize {
        self.src_to_dst.len()
    }

    fn capacity(&self) -> (usize, usize) {
        (self.src_to_dst.len(), self.dst_to_src.len())
    }

    fn link(&mut self, src: T, dst: T) {
        // assert_eq!(self.src_to_dst[src.to_usize().unwrap()], zero()); // maybe too strong req
        // assert_eq!(self.dst_to_src[dst.to_usize().unwrap()], zero()); // maybe too strong req
        self.src_to_dst.insert(src, dst);
        self.dst_to_src.insert(dst, src);
    }

    fn cut(&mut self, src: T, dst: T) {
        self.src_to_dst.remove(&src);
        self.dst_to_src.remove(&dst);
    }

    fn is_src(&self, src: &T) -> bool {
        self.src_to_dst.contains_key(src)
    }

    fn is_dst(&self, dst: &T) -> bool {
        self.dst_to_src.contains_key(dst)
    }

    fn topit(&mut self, _left: usize, _right: usize) {}

    fn has(&self, src: &Self::Src, dst: &Self::Src) -> bool {
        self.src_to_dst.contains_key(src) && self.dst_to_src.contains_key(dst)
    }
}

impl<T: PrimInt + Debug + Hash> MonoMappingStore for HashStore<T> {
    fn get_src_unchecked(&self, dst: &T) -> T {
        *self.dst_to_src.get(dst).unwrap()
    }

    fn get_dst_unchecked(&self, src: &T) -> T {
        *self.src_to_dst.get(src).unwrap()
    }

    fn get_src(&self, dst: &T) -> Option<T> {
        self.dst_to_src.get(dst).cloned()
    }

    fn get_dst(&self, src: &T) -> Option<T> {
        self.src_to_dst.get(src).cloned()
    }

    fn link_if_both_unmapped(&mut self, t1: T, t2: T) -> bool {
        if !self.is_src(&t1) && !self.is_dst(&t2) {
            self.link(t1, t2);
            true
        } else {
            false
        }
    }

    type Iter<'a>
        = HMIter<'a, T, T>
    where
        Self: 'a;

    fn iter(&self) -> Self::Iter<'_> {
        HMIter {
            v: self.src_to_dst.iter(),
        }
    }
}

pub struct HMIter<'a, T: 'a + PrimInt, U: 'a> {
    v: hyperast::compat::hash_map::Iter<'a, T, U>,
}

impl<T: PrimInt, U: PrimInt> Iterator for HMIter<'_, T, U> {
    type Item = (T, U);

    fn next(&mut self) -> Option<Self::Item> {
        let (x, y) = self.v.next()?;
        Some((*x, *y))
    }
}
