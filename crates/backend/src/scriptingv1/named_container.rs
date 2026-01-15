use rhai::Dynamic;

use super::finalize::Finalize;

#[derive(Clone)]
pub(super) struct NamedContainer<T> {
    name: String,
    pub(super) content: T,
}

impl<T> NamedContainer<T> {
    pub(super) fn new(name: String, content: T) -> Self {
        Self { name, content }
    }
}

impl Finalize for NamedContainer<Dynamic> {
    type Output = rhai::Array;
    fn finalize(self) -> Self::Output {
        vec![self.name.into(), self.content.finalize()]
    }
}
