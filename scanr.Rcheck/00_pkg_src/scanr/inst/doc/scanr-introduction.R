## -----------------------------------------------------------------------------
#| label: setup
#| include: false
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  fig.width = 7,
  fig.height = 4
)


## -----------------------------------------------------------------------------
library(scanr)


## -----------------------------------------------------------------------------
set.seed(123)
x <- c(rnorm(100, mean = 0), rnorm(100, mean = 2))


## -----------------------------------------------------------------------------
fit <- scan_cpd(
  x,
  window_sizes = c(20L, 30L),
  n_boot = 100L,
  random_state = 123L,
  change_type = "mean",
  n_jobs = 1L
)

fit
fit$change_points


## -----------------------------------------------------------------------------
cpd_metrics(
  true_cps = 100L,
  estimated_cps = fit$change_points,
  n = length(x),
  tolerance = 20L
)


## -----------------------------------------------------------------------------
ts_cusum(x[70:130])
ts_wasserstein(x[70:130])
one_wasserstein_distance(x[1:80], x[121:200])
ipm_statistic(x[1:80], x[121:200])


## -----------------------------------------------------------------------------
vis_change_points(x, fit, true_change_points = 100L)
vis_vote_scree(fit)
vis_window_votes(fit)

