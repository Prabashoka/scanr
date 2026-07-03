.scanr_parse <- function(payload) {
  out <- jsonlite::fromJSON(payload, simplifyVector = FALSE)
  if (!isTRUE(out$ok)) {
    stop(out$error %||% "scanr Rust backend returned an unknown error", call. = FALSE)
  }
  out$result
}

`%||%` <- function(x, y) {
  if (is.null(x)) y else x
}

.as_numeric_series <- function(x) {
  x <- as.numeric(x)
  if (length(x) < 3L) {
    stop("x must contain at least 3 observations", call. = FALSE)
  }
  if (any(!is.finite(x))) {
    stop("x contains NA, NaN, or infinite values", call. = FALSE)
  }
  x
}

.choose_default_windows <- function(n, min_window, max_window) {
  min_window <- as.integer(min_window)
  if (min_window <= 0L) {
    stop("min_window must be positive", call. = FALSE)
  }

  upper <- if (is.null(max_window)) floor(sqrt(n)) else as.integer(max_window)
  upper <- max(min_window, upper)
  upper <- min(upper, max(1L, floor(n / 2L)))

  if (min_window > upper) {
    stop("min_window is too large for the length of x", call. = FALSE)
  }

  n_grid <- min(5L, upper - min_window + 1L)
  unique(as.integer(round(seq(min_window, upper, length.out = n_grid))))
}

.normalize_windows <- function(window_sizes, n) {
  window_sizes <- sort(unique(as.integer(window_sizes)))
  if (!length(window_sizes)) {
    stop("window_sizes must not be empty", call. = FALSE)
  }
  if (any(window_sizes <= 0L)) {
    stop("window_sizes must contain positive integers", call. = FALSE)
  }
  if (any(2L * window_sizes > n)) {
    stop("each window size must satisfy 2 * window_size <= length(x)", call. = FALSE)
  }
  window_sizes
}

.named_numeric <- function(x) {
  if (is.null(x) || !length(x)) {
    return(setNames(numeric(), character()))
  }
  values <- vapply(x, as.numeric, numeric(1), USE.NAMES = FALSE)
  names(values) <- names(x)
  values
}

.named_integer <- function(x) {
  if (is.null(x) || !length(x)) {
    return(setNames(integer(), character()))
  }
  values <- vapply(x, as.integer, integer(1), USE.NAMES = FALSE)
  names(values) <- names(x)
  values
}

.as_int_vec <- function(x) {
  as.integer(unlist(x, use.names = FALSE))
}

.window_result <- function(info, window_size = NULL) {
  structure(
    list(
      window_size = as.integer(window_size %||% NA_integer_),
      change_points = .as_int_vec(info$change_points),
      starts = .as_int_vec(info$starts),
      statistics = as.numeric(unlist(info$statistics, use.names = FALSE)),
      tapered_block_bootstrap_threshold = as.numeric(unlist(
        info$tapered_block_bootstrap_threshold,
        use.names = FALSE
      )),
      localized_regions = do.call(
        rbind,
        lapply(info$localized_regions, function(z) as.integer(unlist(z, use.names = FALSE)))
      )
    ),
    class = "scanr_window_result"
  )
}

#' Detect change points in a univariate time series
#'
#' @param x Numeric vector.
#' @param window_sizes Optional positive integer vector of scan window sizes.
#' @param alpha Significance level, either as a proportion such as `0.05` or a
#'   percentage such as `5`.
#' @param n_boot Number of tapered block bootstrap replications.
#' @param vote_threshold Minimum normalized ensemble vote score for retained
#'   change points.
#' @param min_window Minimum default window size when `window_sizes` is `NULL`.
#' @param max_window Maximum default window size when `window_sizes` is `NULL`.
#' @param block_length Optional tapered block bootstrap block length.
#' @param taper Taper shape, either `"tukey"` or `"none"`.
#' @param tolerance Distance used to merge nearby candidates across windows.
#' @param random_state Optional non-negative integer seed.
#' @param n_jobs Optional number of Rust/Rayon worker threads. Defaults to 1.
#'   Use `-1` for all detected cores.
#' @param return_all Whether to keep per-window diagnostics and raw output.
#' @param change_type One of `"distribution"`, `"mean"`, or `"var"`.
#' @param eps Small positive value used to avoid division by zero.
#' @param batch_size Bootstrap batch size used by the Rust backend.
#' @return An object of class `scanr_result`.
#' @export
scan_cpd <- function(x,
                     window_sizes = NULL,
                     alpha = 0.05,
                     n_boot = 400L,
                     vote_threshold = 0.5,
                     min_window = 15L,
                     max_window = NULL,
                     block_length = NULL,
                     taper = c("tukey", "none"),
                     tolerance = NULL,
                     random_state = NULL,
                     n_jobs = NULL,
                     return_all = TRUE,
                     change_type = c("distribution", "mean", "var"),
                     eps = 1e-12,
                     batch_size = 32L) {
  x <- .as_numeric_series(x)
  change_type <- match.arg(change_type)
  taper <- match.arg(taper)

  if (is.null(window_sizes)) {
    window_sizes <- .choose_default_windows(length(x), min_window, max_window)
  }
  window_sizes <- .normalize_windows(window_sizes, length(x))

  tolerance <- as.integer(tolerance %||% min(window_sizes))
  seed <- as.integer(random_state %||% 0L)
  workers <- if (is.null(n_jobs)) {
    1L
  } else if (as.integer(n_jobs) == -1L) {
    as.integer(parallel::detectCores(logical = TRUE) %||% 1L)
  } else {
    as.integer(n_jobs)
  }

  taper_ratio <- switch(taper, tukey = 0.5, none = 0)
  block_length <- as.integer(block_length %||% 0L)

  raw <- .scanr_parse(.scan_detector_json(
    x, window_sizes, as.integer(n_boot), as.numeric(alpha), seed, tolerance,
    workers, "thread", change_type, as.numeric(eps), block_length,
    taper_ratio, TRUE, as.integer(batch_size)
  ))

  scores <- .named_numeric(raw$out$leaders_scores)
  votes <- .named_integer(raw$out$leaders_segment_votes)
  change_points <- sort(as.integer(names(scores)[scores >= vote_threshold]))

  window_results <- lapply(names(raw$window_results), function(w) {
    .window_result(raw$window_results[[w]], as.integer(w))
  })
  names(window_results) <- names(raw$window_results)

  thresholds <- lapply(window_results, function(info) {
    list(
      starts = info$starts,
      statistics = info$statistics,
      tapered_block_bootstrap_threshold = info$tapered_block_bootstrap_threshold
    )
  })

  result <- list(
    change_points = change_points,
    scores = scores,
    votes = votes,
    window_results = if (isTRUE(return_all)) window_results else list(),
    thresholds = if (isTRUE(return_all)) thresholds else list(),
    parameters = list(
      window_sizes = window_sizes,
      alpha = alpha,
      n_boot = n_boot,
      vote_threshold = vote_threshold,
      block_length = if (block_length > 0L) block_length else NULL,
      taper = taper,
      change_type = change_type,
      tolerance = tolerance,
      random_state = random_state,
      n_jobs = n_jobs,
      eps = eps,
      batch_size = batch_size
    ),
    metadata = list(n_obs = length(x), index_base = "R split position"),
    segments = raw$segments,
    raw = if (isTRUE(return_all)) raw else list()
  )

  structure(result, class = "scanr_result")
}

#' Run SCAN for one window size
#' @inheritParams scan_cpd
#' @param window_size Positive integer scan window size.
#' @return An object of class `scanr_window_result`.
#' @export
scan_single_window <- function(x,
                               window_size,
                               alpha = 0.05,
                               n_boot = 400L,
                               block_length = NULL,
                               taper = c("tukey", "none"),
                               random_state = NULL,
                               change_type = c("distribution", "mean", "var"),
                               eps = 1e-12,
                               batch_size = 32L) {
  x <- .as_numeric_series(x)
  taper <- match.arg(taper)
  change_type <- match.arg(change_type)
  taper_ratio <- switch(taper, tukey = 0.5, none = 0)

  raw <- .scanr_parse(.scan_single_window_json(
    x, as.integer(window_size), as.integer(n_boot), as.numeric(alpha),
    as.integer(random_state %||% 0L), change_type, as.numeric(eps),
    as.integer(block_length %||% 0L), taper_ratio, TRUE,
    as.integer(batch_size)
  ))

  .window_result(raw, as.integer(window_size))
}

#' Localize a mean change with a CUSUM statistic
#' @param x Numeric vector.
#' @return Integer split position.
#' @export
ts_cusum <- function(x) {
  as.integer(.scanr_parse(.refine_cusum_json(.as_numeric_series(x))))
}

#' Localize a distributional change with a Wasserstein statistic
#' @param x Numeric vector.
#' @return A list with `change_point` and `statistics`.
#' @export
ts_wasserstein <- function(x) {
  out <- .scanr_parse(.refine_wasserstein_json(.as_numeric_series(x)))
  list(
    change_point = as.integer(out$change_point),
    statistics = as.numeric(unlist(out$statistics, use.names = FALSE))
  )
}

#' Local SCAN/Wasserstein split statistic
#' @param x Numeric vector.
#' @param change_type One of `"distribution"`, `"mean"`, or `"var"`.
#' @return Integer split position.
#' @export
swal_statistic <- function(x, change_type = c("distribution", "mean", "var")) {
  change_type <- match.arg(change_type)
  as.integer(.scanr_parse(.swal_statistic_json(.as_numeric_series(x), change_type)))
}

#' One-dimensional Wasserstein distance
#' @param left Numeric vector.
#' @param right Numeric vector.
#' @return Numeric distance.
#' @export
one_wasserstein_distance <- function(left, right) {
  as.numeric(.scanr_parse(.wasserstein_statistic_json(as.numeric(left), as.numeric(right))))
}

#' Integral probability metric statistic
#' @inheritParams one_wasserstein_distance
#' @return Numeric distance.
#' @export
ipm_statistic <- function(left, right) {
  as.numeric(.scanr_parse(.ipm_statistic_json(as.numeric(left), as.numeric(right))))
}

#' @export
print.scanr_result <- function(x, ...) {
  cat("scanr change-point result\n")
  cat("observations:", x$metadata$n_obs, "\n")
  cat("change points:", if (length(x$change_points)) paste(x$change_points, collapse = ", ") else "<none>", "\n")
  invisible(x)
}

#' @export
print.scanr_window_result <- function(x, ...) {
  cat("scanr single-window result\n")
  cat("window size:", x$window_size, "\n")
  cat("change points:", if (length(x$change_points)) paste(x$change_points, collapse = ", ") else "<none>", "\n")
  invisible(x)
}
