# scanr

<img src="man/scan_logo/scanr-logo.png" align="right" width="140" alt="scanr logo">

[![R-CMD-check](https://github.com/Prabashoka/scanr/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/Prabashoka/scanr/actions/workflows/R-CMD-check.yaml)

`scanr` is an R package for sequential change-point detection in univariate
long time series. The R interface calls a native Rust backend through `extendr`.

## Installation

Users can install the package from CRAN and load it as follows:

install.packages("scanr")


# Or the development version from GitHub:
# install.packages("pak")
pak::pak("Prabashoka/scanr")

Because the GitHub version is compiled from source, 
Rust and Cargo must be installed before installing scanr.
To check whether they are available, run:
Sys.which(c("cargo", "rustc"))
Both commands should return a valid file path.
If they are not installed, follow the Rust installation
instructions at https://rustup.rs/.

## Basic Usage

The example below simulates a time series of length 20,000 with 20 change
points in the mean. A scan window contains observations on each side of a
candidate split. Smaller windows localize nearby changes more precisely but
contain less information; larger windows are more stable but should remain
smaller than the spacing between nearby changes.

```r
set.seed(1234)

n <- 20000

change_points <- c(
  952, 1905, 2858, 3810, 4763, 5715, 6668, 7620, 8573, 9525,
  10478, 11430, 12383, 13335, 14288, 15240, 16193, 17145, 18098,
  19050
)

means <- c(
  0, 2, -1, 3, 0.5, -2, 2, 5, -0.5, 2.5, 0, -2.5, -1.5, 1.5,
  3, 1, 0, 1.25, -2, 3.5, -1.5
)

segment_starts <- c(1L, change_points + 1L)
segment_ends <- c(change_points, n)
x_mean <- numeric(n)

for (j in seq_along(means)) {
  segment_index <- segment_starts[j]:segment_ends[j]
  x_mean[segment_index] <- rnorm(length(segment_index), mean = means[j], sd = 1)
}

change_points
```

`default_window_sizes()` samples `n_windows` evenly spaced scales between the
chosen bounds. Its upper bound defaults to `floor(sqrt(n))` and cannot exceed
`floor(n / 2)`. Here we set problem-informed bounds because the simulated
changes are roughly 950 observations apart. If `window_sizes` is omitted,
`scan_cpd()` calls this helper using its `min_window`, `max_window`, and
`n_windows` arguments.

```r
window_sizes <- default_window_sizes(
  n = length(x_mean),
  min_window = 100L,
  max_window = 737L,
  n_windows = 11L
)
window_sizes

fit_mean <- scan_cpd(
  x_mean,
  window_sizes = window_sizes,
  n_boot = 400,
  random_state = 1234,
  change_type = "mean",
  n_jobs = 1
)

fit_mean
```

## Reference

Include the paper here.

## License

This package is free and open source software, licensed under GPL-3.
