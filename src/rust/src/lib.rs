
// Copyright (C) 2026 Ashoka Prabbashwara, Patricia Menéndez, Liam Hodgkinson and Stuart Lee 
//
// This file is part of scanr.
//
// scanr is free software: you can redistribute it and/or modify it
// under the terms of the GNU General Public License as published by
// the Free Software Foundation, either version 3 of the License, or
// any later version.
//
// See the LICENSE file in the project root for the full licence text.

//! Rust backend for the R `scanr` package.
//!
//! The language-neutral implementation lives in the `scan-core` crate.

pub mod r_api;

pub use r_api::*;
