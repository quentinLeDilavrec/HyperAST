use num_traits::{PrimInt, cast, one, zero};
use std::fmt::{Debug, Display};

use super::{MappingStore, MonoMappingStore};

/// TODO try using umax
#[derive(Debug, PartialEq)]
pub struct VecStore<T> {
    pub src_to_dst: Vec<T>,
    pub dst_to_src: Vec<T>,
}

impl<T> Default for VecStore<T> {
    fn default() -> Self {
        Self {
            src_to_dst: Default::default(),
            dst_to_src: Default::default(),
        }
    }
}

impl<T: PrimInt + Debug> VecStore<T> {
    pub fn _iter(&self) -> impl Iterator<Item = (T, T)> + '_ {
        self.src_to_dst
            .iter()
            .enumerate()
            .filter(|x| *x.1 != zero())
            .map(|(src, dst)| (cast::<_, T>(src).unwrap(), *dst - one()))
    }

    pub fn link_if_both_unmapped(&mut self, t1: T, t2: T) -> bool {
        if !self.is_src(&t1) && !self.is_dst(&t2) {
            self.link(t1, t2);
            true
        } else {
            false
        }
    }
}

impl<T> VecStore<T> {
    pub fn mirror(mut self) -> Self {
        std::mem::swap(&mut self.src_to_dst, &mut self.dst_to_src);
        self
    }
}

// struct Iter<T, It:Iterator<Item = (T,T)>> {
//     internal:It,
// }

// impl<T, It:Iterator<Item = (T,T)>> Iterator for Iter<T,It> {
//     type Item = (T,T);

//     fn next(&mut self) -> Option<Self::Item> {
//         todo!()
//     }
// }

impl<T: PrimInt + Debug> Clone for VecStore<T> {
    fn clone(&self) -> Self {
        Self {
            src_to_dst: self.src_to_dst.clone(),
            dst_to_src: self.dst_to_src.clone(),
        }
    }
}

impl<T: PrimInt + Debug> MappingStore for VecStore<T> {
    type Src = T;
    type Dst = T;

    fn len(&self) -> usize {
        self.src_to_dst.iter().filter(|x| **x != zero()).count()
    }

    fn capacity(&self) -> (usize, usize) {
        (self.src_to_dst.len(), self.dst_to_src.len())
    }

    fn link(&mut self, src: T, dst: T) {
        // assert_eq!(self.src_to_dst[src.to_usize().unwrap()], zero()); // maybe too strong req
        // assert_eq!(self.dst_to_src[dst.to_usize().unwrap()], zero()); // maybe too strong req
        self.src_to_dst[src.to_usize().unwrap()] = dst + one();
        self.dst_to_src[dst.to_usize().unwrap()] = src + one();
    }

    fn cut(&mut self, src: T, dst: T) {
        self.src_to_dst[src.to_usize().unwrap()] = zero();
        self.dst_to_src[dst.to_usize().unwrap()] = zero();
    }

    fn is_src(&self, src: &T) -> bool {
        if src.to_usize().unwrap() >= self.src_to_dst.len() {
            return false;
        }
        self.src_to_dst[src.to_usize().unwrap()] != zero()
    }

    fn is_dst(&self, dst: &T) -> bool {
        if dst.to_usize().unwrap() >= self.dst_to_src.len() {
            return false;
        }
        self.dst_to_src[dst.to_usize().unwrap()] != zero()
    }

    fn topit(&mut self, left: usize, right: usize) {
        // let m = left.max(right);
        self.src_to_dst.resize(left + 1, zero());
        self.dst_to_src.resize(right + 1, zero());
    }

    fn has(&self, src: &Self::Src, dst: &Self::Dst) -> bool {
        self.src_to_dst[src.to_usize().unwrap()] == *dst + one()
            && self.dst_to_src[dst.to_usize().unwrap()] == *src + one()
    }
}

impl<T: PrimInt + Debug> MonoMappingStore for VecStore<T> {
    fn get_src_unchecked(&self, dst: &T) -> T {
        self.dst_to_src[dst.to_usize().unwrap()] - one()
    }

    fn get_dst_unchecked(&self, src: &T) -> T {
        self.src_to_dst[src.to_usize().unwrap()] - one()
    }

    fn get_src(&self, dst: &T) -> Option<T> {
        self.dst_to_src
            .get(dst.to_usize().unwrap())
            .and_then(|x| (!x.is_zero()).then(|| *x - one()))
    }

    fn get_dst(&self, src: &T) -> Option<T> {
        self.src_to_dst
            .get(src.to_usize().unwrap())
            .and_then(|x| (!x.is_zero()).then(|| *x - one()))
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
        = MonoIter<'a, T, T>
    where
        Self: 'a;

    fn iter(&self) -> Self::Iter<'_> {
        MonoIter {
            v: self.src_to_dst.iter().enumerate(),
            // .filter(|x|*x.1 != zero()),
            // .map(|(src, dst)| (cast::<_, T>(src).unwrap(), *dst - one())),
            _phantom: std::marker::PhantomData,
        }
    }
}

#[rustfmt::skip]
mod impl_vec_store_mut {
    use num_traits::PrimInt; use std::fmt::Debug;
    use super::{MappingStore, MonoIter, MonoMappingStore, VecStore};
    impl<T: PrimInt + Debug> MappingStore for &mut VecStore<T> {
        type Src = T; type Dst = T;
        fn len(&self) -> usize                         { MappingStore::len(*self)                 }
        fn capacity(&self) -> (usize, usize)           { MappingStore::capacity(*self)            }
        fn link(&mut self, src: T, dst: T)             { MappingStore::link(*self, src, dst);     }
        fn cut(&mut self, src: T, dst: T)              { MappingStore::cut(*self, src, dst);      }
        fn is_src(&self, src: &T) -> bool              { MappingStore::is_src(*self, src)         }
        fn is_dst(&self, dst: &T) -> bool              { MappingStore::is_dst(*self, dst)         }
        fn topit(&mut self, left: usize, right: usize) { MappingStore::topit(*self, left, right); }
        fn has(&self, src: &T, dst: &T) -> bool        { MappingStore::has(*self, src, dst)       }
    }
    impl<T: PrimInt + Debug> MonoMappingStore for &mut VecStore<T> {
        type Iter<'a> = MonoIter<'a, T, T> where Self: 'a;
        fn iter(&self) -> Self::Iter<'_>          { MonoMappingStore::iter(*self)                   }
        fn get_src_unchecked(&self, dst: &T) -> T { MonoMappingStore::get_src_unchecked(*self, dst) }
        fn get_dst_unchecked(&self, src: &T) -> T { MonoMappingStore::get_dst_unchecked(*self, src) }
        fn get_src(&self, dst: &T) -> Option<T>   { MonoMappingStore::get_src(*self, dst)           }
        fn get_dst(&self, src: &T) -> Option<T>   { MonoMappingStore::get_dst(*self, src)           }
        fn link_if_both_unmapped(&mut self, t1: T, t2: T) -> bool {
                                             MonoMappingStore::link_if_both_unmapped(*self, t1, t2) }
    }
}

pub struct MonoIter<'a, T: 'a + PrimInt, U: 'a> {
    v: std::iter::Enumerate<core::slice::Iter<'a, U>>,
    _phantom: std::marker::PhantomData<*const T>,
}

impl<T: PrimInt, U: PrimInt> Iterator for MonoIter<'_, T, U> {
    type Item = (T, U);

    fn next(&mut self) -> Option<Self::Item> {
        let mut a = self.v.next();
        loop {
            let (i, x) = a?;
            if x.to_usize().unwrap() != 0 {
                return Some((cast::<_, T>(i).unwrap(), *x - one()));
            } else {
                a = self.v.next();
            }
        }
    }
}

// Debug/Display related helpers

impl<T: PrimInt + Debug> VecStore<T> {
    pub fn display<'b, Src, Dst>(
        &self,
        src_store: &'b Src,
        dst_store: &'b Dst,
    ) -> DisplayVecStore<'_, 'b, T, Src, Dst> {
        DisplayVecStore {
            mappings: self,
            src_store,
            dst_store,
        }
    }
}

pub struct DisplayVecStore<'a, 'b, T, Src, Dst> {
    mappings: &'a VecStore<T>,
    src_store: &'b Src,
    dst_store: &'b Dst,
}

impl<T: PrimInt + TryFrom<usize>, Src, Dst, D: Display> Display
    for DisplayVecStore<'_, '_, T, Src, Dst>
where
    Src: Fn(T) -> D,
    Dst: Fn(T) -> D,
    <T as TryFrom<usize>>::Error: Debug,
{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        for (i, x) in self.mappings.src_to_dst.iter().enumerate() {
            if !x.is_zero() {
                writeln!(
                    f,
                    "({},{})",
                    &(self.src_store)(i.try_into().unwrap()),
                    &(self.dst_store)(((*x).to_usize().unwrap() - 1).try_into().unwrap())
                )?;
            }
        }
        Ok(())
    }
}
