use std::ops::{Deref, DerefMut};

use bumpalo::Bump;

pub(crate) mod boxed;
pub(crate) mod clone_in;
pub(crate) mod vec;

#[derive(Debug, Default)]
pub(crate) struct Allocator {
    bump: Bump,
}

impl From<Bump> for Allocator {
    fn from(bump: Bump) -> Self {
        Self { bump }
    }
}

impl Deref for Allocator {
    type Target = Bump;

    fn deref(&self) -> &Self::Target {
        &self.bump
    }
}

impl DerefMut for Allocator {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.bump
    }
}

#[cfg(test)]
mod test {
    use std::ops::Deref;

    use bumpalo::Bump;

    use super::*;

    #[test]
    fn test_api() {
        let bump = Bump::new();
        let allocator: Allocator = bump.into();
        {
            _ = allocator.deref();
        }
    }
}
