// #![warn(clippy::all, rust_2018_idioms)]
// #![allow(unused)]

pub mod app;
pub use app::HyperApp;
pub use app::Languages;

mod command;
mod command_palette;
mod platform;
mod results_support;
mod types;
mod utils;
pub mod utils_egui;
pub mod utils_poll;
mod utils_results_batched;

mod edition;
