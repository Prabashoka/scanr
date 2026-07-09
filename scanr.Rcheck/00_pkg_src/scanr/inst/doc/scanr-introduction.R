## -----------------------------------------------------------------------------
#| label: setup
#| include: false
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  fig.width = 7,
  fig.height = 4,
  fig.align = "center"
)


## -----------------------------------------------------------------------------
#| eval: false
# install.packages("scanr")


## -----------------------------------------------------------------------------
library(scanr)


## -----------------------------------------------------------------------------
#| eval: false
# if (!requireNamespace("devtools", quietly = TRUE)) {
#   install.packages("devtools")
# }
# 
# devtools::install_github("Prabashoka/scanr")


## -----------------------------------------------------------------------------
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


## -----------------------------------------------------------------------------
#| echo: false
#| warning: false
#| fig.width: 10
#| fig.height: 4
vis_time_series(
  x_mean,
  x_label = "Time",
  y_label = "Value",
  title = ""
)


## -----------------------------------------------------------------------------
fit_mean <- scan_cpd(
  x_mean,
  window_sizes = c(100, 164, 227, 291, 355, 418, 482, 546, 609, 673, 737),
  n_boot = 400,
  random_state = 1234,
  change_type = "mean",
  n_jobs = 1
)

fit_mean


## -----------------------------------------------------------------------------
fit_mean$change_points


## -----------------------------------------------------------------------------
cpd_metrics(
  true_cps = change_points,
  estimated_cps = fit_mean$change_points,
  n = length(x_mean),
  tolerance = 20
)


## -----------------------------------------------------------------------------
#| fig.width: 10
#| fig.height: 4.5
vis_change_points(
  x_mean,
  fit_mean,
  true_change_points = change_points,
  x_label = "Time",
  y_label = "Value"
)


## -----------------------------------------------------------------------------
vis_vote_scree(fit_mean)


## -----------------------------------------------------------------------------
#| fig.width: 10
#| fig.height: 4.5
vis_window_votes(fit_mean)


## -----------------------------------------------------------------------------
set.seed(1234)

n <- 20000

change_points <- c(952, 1905, 2858, 3810, 4763, 5715, 6668, 7620, 8573, 9525, 10478, 11430, 12383, 13335, 14288, 15240, 16193, 17145, 18098, 19050)

segment_starts <- c(1, change_points + 1)
segment_ends <- c(change_points, n)

families <- c("normal", "exponential", "poisson", "t", "gamma", "uniform", "lognormal", "weibull", "chisq", "beta", "normal", "poisson", "exponential", "uniform", "gamma", "t", "lognormal", "beta", "weibull", "chisq", "normal")

scale_factors <- c(0.7, 1.4, 0.6, 1.8, 0.75, 2.5, 0.65, 3.0, 0.7, 2.6, 0.6, 1.7, 0.65, 2.9, 0.7, 2.8, 0.6, 2.6, 0.65, 3.0, 0.7)

simulate_base <- function(m, family) {
  z <- switch(family, normal = rnorm(m, mean = 0, sd = 1), exponential = rexp(m, rate = 1), poisson = rpois(m, lambda = 2), gamma = rgamma(m, shape = 2, rate = 1), uniform = runif(m, min = -sqrt(3), max = sqrt(3)), t = rt(m, df = 3), lognormal = rlnorm(m, meanlog = 0, sdlog = 0.7), beta = rbeta(m, shape1 = 2, shape2 = 5), weibull = rweibull(m, shape = 1.5, scale = 1), chisq = rchisq(m, df = 5))
  as.numeric((z - mean(z)) / sd(z))
}

x_dist <- numeric(n)

for (j in seq_along(families)) {
  segment_index <- segment_starts[j]:segment_ends[j]
  x_dist[segment_index] <-scale_factors[j] * simulate_base(length(segment_index), families[j])
}

change_points


## -----------------------------------------------------------------------------
#| echo: false
#| warning: false
#| fig.width: 10
#| fig.height: 4
vis_time_series(
  x_dist,
  x_label = "Time",
  y_label = "Value",
  title = ""
)


## -----------------------------------------------------------------------------
fit_dist <- scan_cpd(
  x_dist,
  window_sizes = c(100, 145, 191, 236, 282, 327, 373, 418, 464, 509, 555, 600, 646, 691, 737),
  n_boot = 1000,
  random_state = 1234,
  change_type = "distribution",
  vote_threshold = 0.5,
  n_jobs = 1
)


## -----------------------------------------------------------------------------
fit_dist$change_points


## -----------------------------------------------------------------------------
cpd_metrics(
  true_cps = change_points,
  estimated_cps = fit_dist$change_points,
  n = length(x_dist),
  tolerance = 20
)


## -----------------------------------------------------------------------------
#| fig.width: 10
#| fig.height: 4.5
vis_change_points(
  x_dist,
  fit_dist,
  true_change_points = change_points,
  x_label = "Time",
  y_label = "Value"
)


## -----------------------------------------------------------------------------
set.seed(1234)

true_single_cp <- 150
mean_region <- c(
  rnorm(true_single_cp, mean = 0, sd = 1),
  rnorm(true_single_cp, mean = 2, sd = 1)
)

c(
  truth = true_single_cp,
  cusum = ts_cusum(mean_region),
  swal_distribution = swal_statistic(mean_region, change_type = "distribution")
)


## -----------------------------------------------------------------------------
set.seed(1234)

var_region <- c(
  rnorm(true_single_cp, mean = 0, sd = 0.5),
  rnorm(true_single_cp, mean = 0, sd = 2)
)

c(
  truth = true_single_cp,
  cusum = ts_cusum(var_region),
  swal_distribution = swal_statistic(var_region, change_type = "distribution")
)


## -----------------------------------------------------------------------------
vis_swal_curve(
  var_region,
  start = 1,
  end = length(var_region),
  x_label = "Candidate split",
  y_label = "Scaled Wasserstein statistic",
  title = ""
)


## -----------------------------------------------------------------------------
sessionInfo()

