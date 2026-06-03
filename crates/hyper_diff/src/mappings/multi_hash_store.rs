#![allow(unused)]
use num_traits::PrimInt;
use std::fmt::Debug;
use std::hash::Hash;

use hyperast::compat::HashMap;

use super::{MappingStore, MultiMappingStore};

#[derive(Debug)]
pub struct MultiHashStore<T> {
    pub src_to_dst: Vec<HashMap<T, T>>,
    pub dst_to_src: Vec<HashMap<T, T>>,
}

impl<T> Default for MultiHashStore<T> {
    fn default() -> Self {
        Self {
            src_to_dst: vec![Default::default()],
            dst_to_src: vec![Default::default()],
        }
    }
}

impl<T: PrimInt + Debug> Clone for MultiHashStore<T> {
    fn clone(&self) -> Self {
        Self {
            src_to_dst: self.src_to_dst.clone(),
            dst_to_src: self.dst_to_src.clone(),
        }
    }
}

impl<T: PrimInt + Debug + Hash> MappingStore for MultiHashStore<T> {
    type Src = T;
    type Dst = T;

    fn len(&self) -> usize {
        self.src_to_dst.len()
    }

    fn capacity(&self) -> (usize, usize) {
        (self.src_to_dst.len(), self.dst_to_src.len())
    }

    fn link(&mut self, src: T, dst: T) {
        fn aux<T: Hash + Eq>(vec: &mut Vec<HashMap<T, T>>, target: T, other: T) {
            let mut i = 0;
            while let Some(o) = vec[i].get(&target) {
                if *o == other {
                    return;
                }
                i += 1;
                if i >= vec.len() {
                    vec.push(Default::default());
                }
            }
            vec[i].insert(target, other);
        }
        aux(&mut self.src_to_dst, src, dst);
        aux(&mut self.dst_to_src, dst, src);
    }

    fn cut(&mut self, src: T, dst: T) {
        fn aux<T: PrimInt + Hash>(vec: &mut [HashMap<T, T>], target: T, other: T) {
            let mut i = 0;
            let len = vec.len();
            let _j = loop {
                use hyperast::compat::hash_map::Entry;
                let Entry::Occupied(entry) = vec[i].entry(target) else {
                    return;
                };
                if &other == entry.get() {
                    if i + 1 >= len {
                        entry.remove();
                        return;
                    }
                    break i;
                }
                i += 1;
                if i >= len {
                    return;
                }
            };
            todo!("continue implementation")
        }
        aux(&mut self.src_to_dst, src, dst);
        aux(&mut self.dst_to_src, dst, src);
    }

    fn is_src(&self, _src: &T) -> bool {
        todo!()
        // self.src_to_dst.contains_key(src)
    }

    fn is_dst(&self, _dst: &T) -> bool {
        todo!()
        // self.dst_to_src.contains_key(dst)
    }

    fn topit(&mut self, _left: usize, _right: usize) {}

    fn has(&self, _src: &Self::Src, _dst: &Self::Src) -> bool {
        todo!()
        // self.src_to_dst.contains_key(src) && self.dst_to_src.contains_key(dst)
    }
}

impl<T: PrimInt + Debug + Hash> MultiMappingStore for MultiHashStore<T> {
    type Iter1<'a>
        = Iter<'a, T>
    where
        T: 'a;
    type Iter2<'a>
        = Iter<'a, T>
    where
        T: 'a;
    fn get_srcs(&self, _dst: &Self::Dst) -> &[Self::Src] {
        todo!()
    }

    fn get_dsts(&self, _src: &Self::Src) -> &[Self::Dst] {
        todo!()
    }

    fn all_mapped_srcs(&self) -> Iter<'_, Self::Src> {
        Iter { v: todo!() }
    }

    fn all_mapped_dsts(&self) -> Iter<'_, Self::Dst> {
        Iter { v: todo!() }
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
        todo!()
    }
}
