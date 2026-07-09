library(scanr)

set.seed(123)
x <- c(rnorm(60), rnorm(60, mean = 1.5))

fit <- scan_cpd(
  x,
  window_sizes = c(15L, 20L),
  n_boot = 5L,
  random_state = 123L,
  change_type = "mean",
  n_jobs = 1L
)

stopifnot(inherits(fit, "scanr_result"))
stopifnot(is.integer(fit$change_points))

metrics <- cpd_metrics(
  true_cps = 60L,
  estimated_cps = fit$change_points,
  n = length(x),
  tolerance = 20L
)
stopifnot(is.list(metrics))
stopifnot(is.numeric(metrics$f1))
stopifnot(metrics$f1 >= 0, metrics$f1 <= 1)

stopifnot(is.numeric(one_wasserstein_distance(x[1:20], x[80:100])))
stopifnot(is.integer(ts_cusum(x)))

png_file <- tempfile(fileext = ".png")
png(png_file)
print(vis_change_points(x, fit, true_change_points = 60L))
print(vis_vote_scree(fit))
print(vis_window_votes(fit))
print(vis_thresholds(fit, window_size = 15L))
dev.off()
unlink(png_file)
