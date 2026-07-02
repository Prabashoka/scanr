use crate::detect::{detect_for_window, run_scan_detector};
use crate::refine::{refine_cp_cusum, refine_cp_wasserstein, refine_for_change_type};
use crate::stats::PrefixStats;
use crate::types::{ChangeType, ScanRustResult};
use crate::validation::validate_series;
use crate::wasserstein::wasserstein_1d;
use extendr_api::prelude::*;
use serde::Serialize;
use serde_json::json;

fn json_result<T: Serialize>(result: ScanRustResult<T>) -> String {
    match result {
        Ok(value) => json!({ "ok": true, "result": value }).to_string(),
        Err(message) => json!({ "ok": false, "error": message }).to_string(),
    }
}

fn positive_usize(value: i32, name: &str) -> ScanRustResult<usize> {
    if value <= 0 {
        Err(format!("{name} must be positive"))
    } else {
        Ok(value as usize)
    }
}

fn nonnegative_u64(value: i32, name: &str) -> ScanRustResult<u64> {
    if value < 0 {
        Err(format!("{name} must be non-negative"))
    } else {
        Ok(value as u64)
    }
}

fn optional_positive_usize(value: i32, name: &str) -> ScanRustResult<Option<usize>> {
    if value <= 0 {
        Ok(None)
    } else {
        positive_usize(value, name).map(Some)
    }
}

fn optional_workers(value: i32) -> ScanRustResult<Option<usize>> {
    if value <= 0 {
        Ok(None)
    } else {
        Ok(Some(value as usize))
    }
}

fn usize_vec(values: Vec<i32>, name: &str) -> ScanRustResult<Vec<usize>> {
    values
        .into_iter()
        .map(|value| positive_usize(value, name))
        .collect()
}

#[derive(Serialize)]
struct RefineWassersteinResult {
    change_point: usize,
    statistics: Vec<f64>,
}

#[extendr]
pub fn scan_detector_json(
    series: Vec<f64>,
    window_sizes: Vec<i32>,
    n_boot: i32,
    alpha: f64,
    seed: i32,
    tolerance: i32,
    workers: i32,
    backend: &str,
    change_type: &str,
    eps: f64,
    block_length: i32,
    taper_ratio: f64,
    center: bool,
    batch_size: i32,
) -> String {
    json_result((|| {
        run_scan_detector(
            series,
            Some(usize_vec(window_sizes, "window_sizes")?),
            positive_usize(n_boot, "n_boot")?,
            alpha,
            nonnegative_u64(seed, "seed")?,
            positive_usize(tolerance, "tolerance")?,
            optional_workers(workers)?,
            backend,
            change_type,
            eps,
            optional_positive_usize(block_length, "block_length")?,
            taper_ratio,
            center,
            positive_usize(batch_size, "batch_size")?,
        )
    })())
}

#[extendr]
pub fn scan_single_window_json(
    series: Vec<f64>,
    window_size: i32,
    n_boot: i32,
    alpha: f64,
    seed: i32,
    change_type: &str,
    eps: f64,
    block_length: i32,
    taper_ratio: f64,
    center: bool,
    batch_size: i32,
) -> String {
    json_result((|| {
        validate_series(&series)?;
        let w = positive_usize(window_size, "window_size")?;
        if 2 * w > series.len() {
            return Err("window_size must satisfy 2 * window_size <= length(x)".to_string());
        }

        let prefix = PrefixStats::from_series(&series);
        let (_, result) = detect_for_window(
            &series,
            &prefix,
            w,
            positive_usize(n_boot, "n_boot")?,
            if alpha <= 1.0 { 100.0 * alpha } else { alpha },
            nonnegative_u64(seed, "seed")?,
            ChangeType::parse(change_type)?,
            eps,
            optional_positive_usize(block_length, "block_length")?,
            taper_ratio,
            center,
            positive_usize(batch_size, "batch_size")?,
        )?;
        Ok(result)
    })())
}

#[extendr]
pub fn refine_cusum_json(series: Vec<f64>) -> String {
    json_result((|| {
        validate_series(&series)?;
        refine_cp_cusum(&series)
    })())
}

#[extendr]
pub fn refine_wasserstein_json(series: Vec<f64>) -> String {
    json_result((|| {
        validate_series(&series)?;
        let (change_point, statistics) = refine_cp_wasserstein(&series)?;
        Ok(RefineWassersteinResult {
            change_point,
            statistics,
        })
    })())
}

#[extendr]
pub fn swal_statistic_json(series: Vec<f64>, change_type: &str) -> String {
    json_result((|| {
        validate_series(&series)?;
        refine_for_change_type(&series, ChangeType::parse(change_type)?)
    })())
}

#[extendr]
pub fn wasserstein_statistic_json(left: Vec<f64>, right: Vec<f64>) -> String {
    json_result((|| {
        if left.is_empty() || right.is_empty() {
            return Err("left and right samples must be non-empty".to_string());
        }
        if left.iter().chain(right.iter()).any(|x| !x.is_finite()) {
            return Err("samples contain NaN or infinite values".to_string());
        }
        Ok(wasserstein_1d(&left, &right))
    })())
}

#[extendr]
pub fn ipm_statistic_json(left: Vec<f64>, right: Vec<f64>) -> String {
    wasserstein_statistic_json(left, right)
}

extendr_module! {
    mod scanr;
    fn scan_detector_json;
    fn scan_single_window_json;
    fn refine_cusum_json;
    fn refine_wasserstein_json;
    fn swal_statistic_json;
    fn wasserstein_statistic_json;
    fn ipm_statistic_json;
}
