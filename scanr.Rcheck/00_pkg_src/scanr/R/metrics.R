.clean_cps <- function(cps) {
  if (is.null(cps) || !length(cps)) {
    return(integer())
  }
  sort(unique(as.integer(cps)))
}

#' Match true and estimated change points
#'
#' Greedily matches true and estimated change points by smallest distance,
#' allowing each point to be used at most once.
#'
#' @param true_cps Integer vector of true change points.
#' @param estimated_cps Integer vector of estimated change points.
#' @param tolerance Maximum absolute distance allowed for a match.
#' @return A data frame with columns `true`, `estimated`, and `distance`.
#' @export
match_change_points <- function(true_cps, estimated_cps, tolerance = 10L) {
  true <- .clean_cps(true_cps)
  estimated <- .clean_cps(estimated_cps)
  tolerance <- as.integer(tolerance)

  if (tolerance < 0L) {
    stop("tolerance must be non-negative", call. = FALSE)
  }

  if (!length(true) || !length(estimated)) {
    return(data.frame(
      true = integer(),
      estimated = integer(),
      distance = integer()
    ))
  }

  candidates <- expand.grid(
    true = true,
    estimated = estimated,
    KEEP.OUT.ATTRS = FALSE
  )
  candidates$distance <- abs(candidates$true - candidates$estimated)
  candidates <- candidates[candidates$distance <= tolerance, , drop = FALSE]
  candidates <- candidates[order(candidates$distance, candidates$true, candidates$estimated), , drop = FALSE]

  used_true <- integer()
  used_estimated <- integer()
  rows <- vector("list", 0L)

  for (i in seq_len(nrow(candidates))) {
    candidate <- candidates[i, , drop = FALSE]
    if (candidate$true %in% used_true || candidate$estimated %in% used_estimated) {
      next
    }
    rows[[length(rows) + 1L]] <- candidate
    used_true <- c(used_true, candidate$true)
    used_estimated <- c(used_estimated, candidate$estimated)
  }

  if (!length(rows)) {
    return(data.frame(
      true = integer(),
      estimated = integer(),
      distance = integer()
    ))
  }

  out <- do.call(rbind, rows)
  rownames(out) <- NULL
  out
}

#' Tolerant precision and recall for change-point detection
#'
#' @inheritParams match_change_points
#' @return Named numeric vector with `precision` and `recall`.
#' @export
precision_recall_cpd <- function(true_cps, estimated_cps, tolerance = 10L) {
  true <- .clean_cps(true_cps)
  estimated <- .clean_cps(estimated_cps)
  matches <- match_change_points(true, estimated, tolerance = tolerance)

  precision <- if (length(estimated)) nrow(matches) / length(estimated) else as.numeric(!length(true))
  recall <- if (length(true)) nrow(matches) / length(true) else as.numeric(!length(estimated))

  c(precision = precision, recall = recall)
}

#' Tolerant F1 score for change-point detection
#'
#' @inheritParams match_change_points
#' @return Numeric F1 score.
#' @export
f1_score_cpd <- function(true_cps, estimated_cps, tolerance = 10L) {
  pr <- precision_recall_cpd(true_cps, estimated_cps, tolerance = tolerance)
  if (sum(pr) == 0) {
    return(0)
  }
  2 * pr[["precision"]] * pr[["recall"]] / sum(pr)
}

.segments_from_cps <- function(cps, n) {
  valid <- cps[cps > 0L & cps < n]
  bounds <- c(0L, valid, n)
  cbind(start = head(bounds, -1L), end = tail(bounds, -1L))
}

#' Segment covering metric
#'
#' Computes a weighted segment-covering score in `[0, 1]`.
#'
#' @param true_cps Integer vector of true change points.
#' @param estimated_cps Integer vector of estimated change points.
#' @param n Number of observations in the series.
#' @return Numeric covering score.
#' @export
covering_metric <- function(true_cps, estimated_cps, n) {
  n <- as.integer(n)
  if (length(n) != 1L || is.na(n) || n <= 0L) {
    stop("n must be a positive integer", call. = FALSE)
  }

  true_segments <- .segments_from_cps(.clean_cps(true_cps), n)
  estimated_segments <- .segments_from_cps(.clean_cps(estimated_cps), n)

  total <- 0
  for (i in seq_len(nrow(true_segments))) {
    true_start <- true_segments[i, "start"]
    true_end <- true_segments[i, "end"]
    true_len <- true_end - true_start
    best <- 0

    for (j in seq_len(nrow(estimated_segments))) {
      est_start <- estimated_segments[j, "start"]
      est_end <- estimated_segments[j, "end"]
      overlap <- max(0, min(true_end, est_end) - max(true_start, est_start))
      union <- max(true_end, est_end) - min(true_start, est_start)
      if (union > 0) {
        best <- max(best, overlap / union)
      }
    }

    total <- total + true_len * best
  }

  unname(total / n)
}

#' Combined change-point accuracy metrics
#'
#' @inheritParams covering_metric
#' @param tolerance Maximum absolute distance allowed for a tolerant match.
#' @return A list with matches, precision, recall, F1, and covering score.
#' @export
cpd_metrics <- function(true_cps, estimated_cps, n, tolerance = 10L) {
  matches <- match_change_points(true_cps, estimated_cps, tolerance = tolerance)
  pr <- precision_recall_cpd(true_cps, estimated_cps, tolerance = tolerance)

  list(
    matches = matches,
    precision = unname(pr[["precision"]]),
    recall = unname(pr[["recall"]]),
    f1 = f1_score_cpd(true_cps, estimated_cps, tolerance = tolerance),
    covering = covering_metric(true_cps, estimated_cps, n = n)
  )
}
