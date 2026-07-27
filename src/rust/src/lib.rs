//! Rust backend for the R `scanr` package.
//!
//! The language-neutral implementation lives in the `scan-core` crate.

pub mod r_api;

pub use r_api::*;
