test_that("change points are matched one-to-one within tolerance", {
  metrics <- cpd_metrics(
    true_cps = c(10, 50),
    estimated_cps = c(12, 47, 90),
    n = 100,
    tolerance = 3
  )

  expect_equal(
    metrics$matches,
    data.frame(
      true = c(10, 50),
      estimated = c(12, 47),
      distance = c(2, 3)
    )
  )
  expect_equal(metrics$precision, 2 / 3)
  expect_equal(metrics$recall, 1)
  expect_equal(metrics$f1, 0.8)
})

test_that("duplicate change points do not inflate accuracy", {
  expect_equal(
    precision_recall_cpd(c(10, 10), c(10), tolerance = 0),
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
  expect_equal(f1_score_cpd(10, integer()), 0)
})

test_that("covering metric has known boundary values", {
  expect_equal(covering_metric(c(25, 75), c(25, 75), n = 100), 1)
  expect_equal(covering_metric(5, integer(), n = 10), 0.5)
  expect_error(covering_metric(integer(), integer(), n = 0), "positive integer")
})
