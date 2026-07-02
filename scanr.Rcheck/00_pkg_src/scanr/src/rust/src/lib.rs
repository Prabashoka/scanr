//! Rust backend for the R `scanr` package.
//!
//! The implementation is split into small modules:
//! - `stats`: prefix sums and simple summaries
//! - `wasserstein`: 1D Wasserstein distance
//! - `bootstrap`: tapered block bootstrap thresholding
//! - `refine`: local change-point refinement
//! - `detect`: main window-based scan engine
//! - `aggregate`: ensemble/voting aggregation
//! - `r_api`: R-facing extendr functions

mod aggregate;
mod bootstrap;
mod detect;
pub mod r_api;
mod refine;
mod stats;
mod types;
mod validation;
mod wasserstein;

pub use r_api::*;
