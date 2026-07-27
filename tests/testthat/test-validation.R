test_that("series validation rejects unusable input", {
  expect_error(ts_cusum(c(1, 2)), "at least 3 observations")
  expect_error(ts_cusum(c(1, NA, 3)), "NA, NaN, or infinite")
  expect_error(ts_cusum(c(1, Inf, 3)), "NA, NaN, or infinite")
})

test_that("window validation normalizes valid windows", {
  expect_equal(
    scanr:::.normalize_windows(c(5L, 3L, 5L), n = 20L),
    c(3L, 5L)
  )
  expect_error(scanr:::.normalize_windows(integer(), 20L), "must not be empty")
  expect_error(scanr:::.normalize_windows(c(0L, 3L), 20L), "positive integers")
  expect_error(scanr:::.normalize_windows(6L, 10L), "2 \\* window_size")
})

test_that("default windows respect the series length", {
  set.seed(123)
  windows <- default_window_sizes(
    n = 100L,
    min_window = 5L,
    max_window = 20L
  )

  expect_true(all(diff(windows) > 0L))
  expect_gte(min(windows), 5L)
  expect_lte(max(windows), 20L)
  sampled <- default_window_sizes(
    n = 1000L,
    min_window = 20L,
    max_window = 100L,
    n_windows = 9L
  )
  expect_length(sampled, 9L)
  expect_true(all(diff(sampled) > 0L))
  expect_true(all(sampled >= 20L & sampled <= 100L))

  first <- default_window_sizes(1000L, 20L, 100L, 9L, seed = 456L)
  expect_equal(
    default_window_sizes(1000L, 20L, 100L, 9L, seed = 456L),
    first
  )
  expect_error(
    default_window_sizes(n = 10L, min_window = 6L, max_window = NULL),
    "too large"
  )
  expect_error(default_window_sizes(n = 0L), "positive integer")
  expect_error(default_window_sizes(n = 100L, max_window = 0L), "positive or NULL")
  expect_error(default_window_sizes(n = 100L, n_windows = 0L), "must be positive")
  expect_error(default_window_sizes(n = 100L, seed = -1L), "non-negative integer")
})

test_that("default maximum window uses n^(2/3)", {
  set.seed(789)
  windows <- default_window_sizes(
    n = 1000L,
    min_window = 95L,
    n_windows = 100L
  )

  expect_equal(windows, 95:100)
})

test_that("Wasserstein samples must be finite and non-empty", {
  expect_error(one_wasserstein_distance(numeric(), 1), "non-empty")
  expect_error(one_wasserstein_distance(1, NA_real_), "NaN or infinite")
})
