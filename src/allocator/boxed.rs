use std::{
    fmt::{self, Debug},
    hash::{self, Hash},
    marker::PhantomData,
    ops::{self, Deref},
    ptr::{self, NonNull},
};

use crate::allocator::Allocator;

pub(crate) struct Box<'a, T: ?Sized>(NonNull<T>, PhantomData<(&'a (), T)>);

impl<'a, T> Box<'a, T> {
    pub(crate) fn new_in(value: T, allocator: &'a Allocator) -> Self {
        Self(NonNull::from(allocator.alloc(value)), PhantomData)
    }

    pub(crate) fn unbox(self) -> T {
        unsafe { ptr::read(self.0.as_ptr()) }
    }

    #[allow(unsafe_code)]
    pub(crate) const unsafe fn dangling() -> Self {
        Self(NonNull::dangling(), PhantomData)
    }
}

impl<'a, T: ?Sized> Box<'a, T> {
    #[inline]
    pub(crate) const unsafe fn from_non_null(ptr: NonNull<T>) -> Self {
        Self(ptr, PhantomData)
    }

    #[inline]
    pub fn into_non_null(boxed: Self) -> NonNull<T> {
        boxed.0
    }
}

impl<'a, T: ?Sized> ops::Deref for Box<'a, T> {
    type Target = T;

    fn deref(&self) -> &Self::Target {
        unsafe { self.0.as_ref() }
    }
}

impl<'a, T: ?Sized> ops::DerefMut for Box<'a, T> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        unsafe { self.0.as_mut() }
    }
}

impl<'a, T: ?Sized> AsRef<T> for Box<'a, T> {
    fn as_ref(&self) -> &T {
        self
    }
}

impl<'a, T: ?Sized> AsMut<T> for Box<'a, T> {
    fn as_mut(&mut self) -> &mut T {
        self
    }
}

impl<'a, T: ?Sized + Debug> Debug for Box<'a, T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.deref().fmt(f)
    }
}

impl<'a, T: Hash> Hash for Box<'a, T> {
    fn hash<H: hash::Hasher>(&self, state: &mut H) {
        self.deref().hash(state);
    }
}

#[cfg(test)]
mod test {
    use std::hash::{DefaultHasher, Hash, Hasher};

    use super::Box;
    use crate::allocator::Allocator;

    #[test]
    fn box_deref_mut() {
        let allocator = Allocator::default();
        let mut b = Box::new_in("x", &allocator);
        let b = &mut *b;
        *b = allocator.alloc("v");

        assert_eq!(*b, "v");
    }

    #[test]
    fn box_debug() {
        let allocator = Allocator::default();
        let b = Box::new_in("x", &allocator);
        let b = format!("{b:?}");

        assert_eq!(b, "\"x\"");
    }

    #[test]
    fn box_hash() {
        fn hash(val: &impl Hash) -> u64 {
            let mut hasher = DefaultHasher::default();
            val.hash(&mut hasher);
            hasher.finish()
        }

        let allocator = Allocator::default();
        let a = Box::new_in("x", &allocator);
        let b = Box::new_in("x", &allocator);

        assert_eq!(hash(&a), hash(&b));
    }

    #[test]
    fn lifetime_variance() {
        fn _assert_box_variant_lifetime<'a: 'b, 'b, T>(program: Box<'a, T>) -> Box<'b, T> {
            program
        }
    }
}
