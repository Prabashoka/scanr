# scanr

[![R-CMD-check](https://github.com/Prabashoka/scanr/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/Prabashoka/scanr/actions/workflows/R-CMD-check.yaml)

`scanr` is an R package for sequential change-point detection in univariate
time series. The R interface calls a native Rust backend through `extendr`.

## Installation

From the package root:

```r
install.packages("remotes")
remotes::install_local(".")
```

You need Rust and Cargo available on your `PATH` because the package compiles a
native backend.

## Basic Usage

```r
library(scanr)

set.seed(123)
x <- c(rnorm(150, 0, 1), rnorm(150, 1.8, 1), rnorm(150, -0.7, 1))

fit <- scan_cpd(
  x,
  window_sizes = c(20, 30, 40),
  n_boot = 200,
  random_state = 123,
  change_type = "mean"
)

fit
fit$change_points
fit$scores
```

Run a single scan window:

```r
one_window <- scan_single_window(
  x,
  window_size = 30,
  n_boot = 200,
  random_state = 123,
  change_type = "mean"
)

one_window
```

Refine candidate change points or compute distances directly:

```r
refine_cusum(x[120:180])
refine_wasserstein(x[120:180])
wasserstein_statistic(x[1:100], x[151:250])
ipm_statistic(x[1:100], x[151:250])
```

## Package Contents

- `R/`: exported R functions and wrappers.
- `src/`: Rust implementation and extendr registration code.
- `DESCRIPTION`, `NAMESPACE`, `LICENSE`, `Cargo.toml`, and `Cargo.lock`: package
  metadata and build configuration.
