#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#| eval: false
install.packages("scanr")
#
#
#
library(scanr)
#
#
#
#
#
#
#
#
#
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
#
#
#
#
#
fit_mean <- scan_cpd(
  x_mean,
  window_sizes = c(100, 164, 227, 291, 355, 418, 482, 546, 609, 673, 737),
  n_boot = 400,
  random_state = 1234,
  change_type = "mean",
  n_jobs = 1
)

fit_mean
#
#
#
#
#
#
#
#
