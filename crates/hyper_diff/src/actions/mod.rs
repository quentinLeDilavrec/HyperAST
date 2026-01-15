#[allow(unused)] // still very experimental
pub mod action_tree;
pub mod action_vec;
pub mod script_generator;
pub mod script_generator2;
pub mod script_generator3;

pub trait Actions {
    fn len(&self) -> usize;
    fn is_empty(&self) -> bool {
        self.len() == 0
    }
}
