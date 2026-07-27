test_that("Wasserstein distance agrees with simple empirical examples", {
  left <- c(0, 2)
  right <- c(1, 3)

  expect_equal(one_wasserstein_distance(left, right), 1)
  expect_equal(one_wasserstein_distance(left, left), 0)
  expect_equal(
    one_wasserstein_distance(left, right),
    one_wasserstein_distance(right, left)
  )
  expect_equal(ipm_statistic(left, right), one_wasserstein_distance(left, right))
})

test_that("refinement statistics localize an unambiguous change", {
  x <- c(rep(0, 5), rep(10, 5))

  expect_identical(ts_cusum(x), 5L)
  expect_identical(swal_statistic(x, change_type = "mean"), 5L)

  wasserstein <- ts_wasserstein(x)
  expect_identical(wasserstein$change_point, 5L)
  expect_length(wasserstein$statistics, length(x) - 1L)
  expect_true(all(is.finite(wasserstein$statistics)))
})

test_that("seeded detector calls are reproducible", {
  set.seed(42)
  x <- c(rnorm(30), rnorm(30, mean = 4))
  args <- list(
    x = x,
    min_window = 10,
    max_window = 12,
    n_windows = 3,
    n_boot = 10,
    random_state = 123,
    change_type = "mean",
    n_jobs = 1
  )

  first <- do.call(scan_cpd, args)
  second <- do.call(scan_cpd, args)

  expect_s3_class(first, "scanr_result")
  expect_identical(first$change_points, second$change_points)
  expect_equal(first$scores, second$scores)
  expect_identical(first$metadata$n_obs, length(x))
  expect_identical(first$metadata$index_base, "R split position")
  expect_identical(
    first$parameters$window_sizes,
    default_window_sizes(
      length(x),
      min_window = 10,
      max_window = 12,
      n_windows = 3
    )
  )
})
