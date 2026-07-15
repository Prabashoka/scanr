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
  windows <- scanr:::.choose_default_windows(
    n = 100L,
    min_window = 5L,
    max_window = 20L
  )

  expect_true(all(diff(windows) > 0L))
  expect_gte(min(windows), 5L)
  expect_lte(max(windows), 20L)
  expect_error(
    scanr:::.choose_default_windows(n = 10L, min_window = 6L, max_window = NULL),
    "too large"
  )
})

test_that("Wasserstein samples must be finite and non-empty", {
  expect_error(one_wasserstein_distance(numeric(), 1), "non-empty")
  expect_error(one_wasserstein_distance(1, NA_real_), "NaN or infinite")
})
