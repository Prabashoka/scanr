test_that("change points are matched one-to-one within tolerance", {
  metrics <- cpd_metrics(
    true_cps = c(10L, 50L),
    estimated_cps = c(12L, 47L, 90L),
    n = 100L,
    tolerance = 3L
  )

  expect_equal(
    metrics$matches,
    data.frame(
      true = c(10L, 50L),
      estimated = c(12L, 47L),
      distance = c(2L, 3L)
    )
  )
  expect_equal(metrics$precision, 2 / 3)
  expect_equal(metrics$recall, 1)
  expect_equal(metrics$f1, 0.8)
})

test_that("duplicate change points do not inflate accuracy", {
  expect_equal(
    precision_recall_cpd(c(10L, 10L), c(10L, 10L), tolerance = 0L),
    c(precision = 1, recall = 1)
  )
})

test_that("empty change-point sets have well-defined scores", {
  expect_equal(
    precision_recall_cpd(integer(), integer()),
    c(precision = 1, recall = 1)
  )
  expect_equal(
    precision_recall_cpd(integer(), 10L),
    c(precision = 0, recall = 0)
  )
  expect_equal(f1_score_cpd(10L, integer()), 0)
})

test_that("covering metric has known boundary values", {
  expect_equal(covering_metric(c(25L, 75L), c(25L, 75L), n = 100L), 1)
  expect_equal(covering_metric(5L, integer(), n = 10L), 0.5)
  expect_error(covering_metric(integer(), integer(), n = 0L), "positive integer")
})
