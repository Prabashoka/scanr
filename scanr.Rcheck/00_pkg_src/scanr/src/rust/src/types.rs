use serde::Serialize;
use std::collections::BTreeMap;

pub(crate) type ScanRustResult<T> = std::result::Result<T, String>;

/// Type of distributional change targeted by the refinement step.
#[derive(Clone, Copy, Debug, Serialize)]
pub(crate) enum ChangeType {
    Mean,
    Var,
    Distribution,
}

impl ChangeType {
    pub(crate) fn parse(value: &str) -> ScanRustResult<Self> {
        match value.to_ascii_lowercase().as_str() {
            "mean" => Ok(Self::Mean),
            "var" => Ok(Self::Var),
            "distribution" => Ok(Self::Distribution),
            other => Err(format!(
                "change_type must be one of {{'mean', 'var', 'distribution'}}, got {other:?}"
            )),
        }
    }
}

/// One merged segment of nearby candidate change-points and their votes.
#[derive(Clone, Debug, Serialize)]
pub(crate) struct SegmentInfo {
    pub(crate) change_points: Vec<usize>,
    pub(crate) votes: BTreeMap<usize, usize>,
    pub(crate) segment_vote: usize,
}

/// Detailed output for a single scan window.
#[derive(Clone, Debug, Serialize)]
pub(crate) struct WindowScanResult {
    pub(crate) change_points: Vec<usize>,
    pub(crate) starts: Vec<usize>,
    pub(crate) statistics: Vec<f64>,
    pub(crate) tapered_block_bootstrap_threshold: Vec<f64>,
    pub(crate) localized_regions: Vec<(usize, usize)>,
}

/// Aggregated voting output returned through the R API.
#[derive(Clone, Debug, Serialize)]
pub(crate) struct AggregatedOut {
    pub(crate) leaders_segment_votes: BTreeMap<usize, usize>,
    pub(crate) leaders_scores: BTreeMap<usize, f64>,
    pub(crate) leaders_probs: BTreeMap<usize, f64>,
    pub(crate) cdf: Vec<(usize, f64)>,
}

/// Full internal scan result before conversion to R-friendly JSON.
#[derive(Clone, Debug, Serialize)]
pub(crate) struct ScanResult {
    pub(crate) cp_dict: BTreeMap<usize, Vec<usize>>,
    pub(crate) window_results: BTreeMap<usize, WindowScanResult>,
    pub(crate) segments: BTreeMap<String, SegmentInfo>,
    pub(crate) out: AggregatedOut,
}
