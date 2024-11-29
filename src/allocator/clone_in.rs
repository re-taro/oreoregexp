use std::cell::Cell;

use crate::allocator::{boxed::Box, vec::Vec, Allocator};

pub trait CloneIn<'alloc>: Sized {
    type Cloned;

    fn clone_in(&self, allocator: &'alloc Allocator) -> Self::Cloned;
}

impl<'alloc, T, C> CloneIn<'alloc> for Option<T>
where
    T: CloneIn<'alloc, Cloned = C>,
{
    type Cloned = Option<C>;

    fn clone_in(&self, allocator: &'alloc Allocator) -> Self::Cloned {
        self.as_ref().map(|it| it.clone_in(allocator))
    }
}

impl<'alloc, T, C> CloneIn<'alloc> for Box<'_, T>
where
    T: CloneIn<'alloc, Cloned = C>,
{
    type Cloned = Box<'alloc, C>;

    fn clone_in(&self, allocator: &'alloc Allocator) -> Self::Cloned {
        Box::new_in(self.as_ref().clone_in(allocator), allocator)
    }
}

impl<'alloc, T, C> CloneIn<'alloc> for Vec<'_, T>
where
    T: CloneIn<'alloc, Cloned = C>,
{
    type Cloned = Vec<'alloc, C>;

    fn clone_in(&self, allocator: &'alloc Allocator) -> Self::Cloned {
        Vec::from_iter_in(self.iter().map(|it| it.clone_in(allocator)), allocator)
    }
}

impl<'alloc, T: Copy> CloneIn<'alloc> for Cell<T> {
    type Cloned = Cell<T>;

    fn clone_in(&self, _: &'alloc Allocator) -> Self::Cloned {
        Cell::new(self.get())
    }
}

impl<'alloc> CloneIn<'alloc> for &str {
    type Cloned = &'alloc str;

    fn clone_in(&self, allocator: &'alloc Allocator) -> Self::Cloned {
        allocator.alloc_str(self)
    }
}

macro_rules! impl_clone_in {
  ($($t:ty)*) => {
      $(
          impl<'alloc> CloneIn<'alloc> for $t {
              type Cloned = Self;
              #[inline(always)]
              fn clone_in(&self, _: &'alloc Allocator) -> Self {
                  *self
              }
          }
      )*
  }
}

impl_clone_in! {
  usize u8 u16 u32 u64 u128
  isize i8 i16 i32 i64 i128
  f32 f64
  bool char
}
