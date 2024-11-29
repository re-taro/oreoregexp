//! Originally based on [jsparagus](https://github.com/mozilla-spidermonkey/jsparagus/blob/24004745a8ed4939fc0dc7332bfd1268ac52285f/crates/ast/src/arena.rs)

use std::{
    fmt::{self, Debug},
    hash::{Hash, Hasher},
    mem::ManuallyDrop,
    ops,
    ptr::NonNull,
};

use allocator_api2::vec;
use bumpalo::Bump;

use crate::allocator::{boxed::Box, Allocator};

#[derive(PartialEq, Eq)]
pub(crate) struct Vec<'alloc, T>(ManuallyDrop<vec::Vec<T, &'alloc Bump>>);

impl<'alloc, T> Vec<'alloc, T> {
    #[inline]
    pub(crate) fn new_in(allocator: &'alloc Allocator) -> Self {
        Self(ManuallyDrop::new(vec::Vec::new_in(allocator)))
    }

    #[inline]
    pub(crate) fn with_capacity_in(capacity: usize, allocator: &'alloc Allocator) -> Self {
        Self(ManuallyDrop::new(vec::Vec::with_capacity_in(
            capacity, allocator,
        )))
    }

    #[inline]
    pub(crate) fn from_iter_in<I: IntoIterator<Item = T>>(
        iter: I,
        allocator: &'alloc Allocator,
    ) -> Self {
        let iter = iter.into_iter();
        let hint = iter.size_hint();
        let capacity = hint.1.unwrap_or(hint.0);
        let mut vec = ManuallyDrop::new(vec::Vec::with_capacity_in(capacity, &**allocator));
        vec.extend(iter);
        Self(vec)
    }

    #[inline]
    pub(crate) fn from_array_in<const N: usize>(
        array: [T; N],
        allocator: &'alloc Allocator,
    ) -> Self {
        let boxed = Box::new_in(array, allocator);
        let ptr = Box::into_non_null(boxed).as_ptr().cast::<T>();
        let vec = unsafe { vec::Vec::from_raw_parts_in(ptr, N, N, &**allocator) };
        Self(ManuallyDrop::new(vec))
    }

    pub(crate) fn into_boxed_slice(self) -> Box<'alloc, [T]> {
        let inner = ManuallyDrop::into_inner(self.0);
        let slice = inner.leak();
        let ptr = NonNull::from(slice);
        unsafe { Box::from_non_null(ptr) }
    }
}

impl<'alloc, T> ops::Deref for Vec<'alloc, T> {
    type Target = vec::Vec<T, &'alloc Bump>;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl<'alloc, T> ops::DerefMut for Vec<'alloc, T> {
    fn deref_mut(&mut self) -> &mut vec::Vec<T, &'alloc Bump> {
        &mut self.0
    }
}

impl<'alloc, T> IntoIterator for Vec<'alloc, T> {
    type IntoIter = <vec::Vec<T, &'alloc Bump> as IntoIterator>::IntoIter;
    type Item = T;

    fn into_iter(self) -> Self::IntoIter {
        let inner = ManuallyDrop::into_inner(self.0);
        inner.into_iter()
    }
}

impl<'alloc, T> IntoIterator for &'alloc Vec<'alloc, T> {
    type IntoIter = std::slice::Iter<'alloc, T>;
    type Item = &'alloc T;

    fn into_iter(self) -> Self::IntoIter {
        self.0.iter()
    }
}

impl<T> ops::Index<usize> for Vec<'_, T> {
    type Output = T;

    fn index(&self, index: usize) -> &Self::Output {
        self.0.index(index)
    }
}

impl<T> ops::IndexMut<usize> for Vec<'_, T> {
    fn index_mut(&mut self, index: usize) -> &mut Self::Output {
        self.0.index_mut(index)
    }
}

impl<T: Hash> Hash for Vec<'_, T> {
    fn hash<H: Hasher>(&self, state: &mut H) {
        for e in self.0.iter() {
            e.hash(state);
        }
    }
}

impl<T: Debug> Debug for Vec<'_, T> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let inner = &*self.0;
        f.debug_tuple("Vec").field(inner).finish()
    }
}

#[cfg(test)]
mod test {
    use super::*;

    use crate::allocator::{boxed::Box, Allocator};

    #[test]
    fn vec_with_capacity() {
        let allocator = Allocator::default();
        let v: Vec<i32> = Vec::with_capacity_in(10, &allocator);
        assert!(v.is_empty());
    }

    #[test]
    fn vec_debug() {
        let allocator = Allocator::default();
        let mut v = Vec::new_in(&allocator);
        v.push("x");
        let v = format!("{v:?}");
        assert_eq!(v, r#"Vec(["x"])"#);
    }

    #[test]
    fn lifetime_variance() {
        fn _assert_vec_variant_lifetime<'a: 'b, 'b, T>(program: Vec<'a, T>) -> Vec<'b, T> {
            program
        }
    }

    #[test]
    fn vec_to_boxed_slice() {
        let allocator = Allocator::default();
        let mut v = Vec::with_capacity_in(10, &allocator);
        v.extend([1, 2, 3]);

        let b = v.into_boxed_slice();
        let b: Box<[u8]> = b;

        assert_eq!(&*b, &[1, 2, 3]);
        assert_eq!(b.len(), 3);
    }
}
