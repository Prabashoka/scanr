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

The example below simulates a series with two changes in mean and then runs
`scan_cpd()` across several local window sizes.

```r
library(scanr)

set.seed(123)

true_cps <- c(200, 400)
x <- c(
  rnorm(200, mean = 0, sd = 1),
  rnorm(200, mean = 2, sd = 1),
  rnorm(200, mean = -1, sd = 1)
)

fit <- scan_cpd(
  x,
  window_sizes = c(40, 60, 80),
  n_boot = 200,
  random_state = 123,
  change_type = "mean",
  n_jobs = 1
)

fit
fit$change_points

cpd_metrics(
  true_cps = true_cps,
  estimated_cps = fit$change_points,
  n = length(x),
  tolerance = 20
)

vis_change_points(
  x,
  fit,
  true_change_points = true_cps,
  x_label = "Time",
  y_label = "Value"
)
```

## Package Contents

- `R/`: exported R functions and wrappers.
- `src/`: Rust implementation and extendr registration code.
- `DESCRIPTION`, `NAMESPACE`, `LICENSE`, `Cargo.toml`, and `Cargo.lock`: package
  metadata and build configuration.
