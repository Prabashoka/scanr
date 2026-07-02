.scanr_blue <- "#00008B"
.scanr_orange <- "#f5710a"
.scanr_grid <- "#D9D9D9"

.plot_series_data <- function(x, index = NULL) {
  y <- as.numeric(x)
  if (!length(y)) {
    stop("x must not be empty", call. = FALSE)
  }
  if (any(!is.finite(y))) {
    stop("x contains NA, NaN, or infinite values", call. = FALSE)
  }

  if (is.null(index)) {
    index <- seq_along(y)
  }
  if (length(index) != length(y)) {
    stop("index must have the same length as x", call. = FALSE)
  }

  data.frame(t = index, value = y)
}

.map_cps_to_axis <- function(cps, index) {
  if (is.null(cps) || !length(cps)) {
    return(index[integer()])
  }

  cps_int <- as.integer(cps)
  mapped <- cps
  inside <- !is.na(cps_int) & cps_int >= 1L & cps_int <= length(index)
  mapped[inside] <- index[cps_int[inside]]
  mapped
}

.plot_scan_style <- function() {
  grid(col = .scanr_grid, lty = "dotted")
  box()
}

#' Plot a time series with optional change-point markers
#'
#' @param x Numeric vector.
#' @param change_points Optional detected change points.
#' @param true_change_points Optional true change points.
#' @param index Optional x-axis index with the same length as `x`.
#' @param x_label X-axis label.
#' @param y_label Y-axis label.
#' @param title Plot title.
#' @param ... Additional arguments passed to `plot()`.
#' @return Invisibly returns the plotted data and mapped change points.
#' @export
plot_time_series <- function(x,
                             change_points = NULL,
                             true_change_points = NULL,
                             index = NULL,
                             x_label = "Time",
                             y_label = "Value",
                             title = "Time series",
                             ...) {
  df <- .plot_series_data(x, index = index)
  detected <- .map_cps_to_axis(change_points, df$t)
  truth <- .map_cps_to_axis(true_change_points, df$t)

  plot(df$t, df$value,
    type = "l", col = .scanr_blue, lwd = 2,
    xlab = x_label, ylab = y_label, main = title, ...
  )
  .plot_scan_style()

  if (length(detected)) {
    abline(v = detected, col = .scanr_orange, lty = "dashed", lwd = 2)
  }
  if (length(truth)) {
    abline(v = truth, col = "black", lty = "dotted", lwd = 2)
  }
  if (length(detected) || length(truth)) {
    legend(
      "topright",
      legend = c(
        if (length(detected)) "Detected change points",
        if (length(truth)) "True change points"
      ),
      col = c(
        if (length(detected)) .scanr_orange,
        if (length(truth)) "black"
      ),
      lty = c(
        if (length(detected)) "dashed",
        if (length(truth)) "dotted"
      ),
      lwd = 2,
      bty = "n"
    )
  }

  invisible(list(data = df, detected = detected, truth = truth))
}

#' Plot detected change points from a scan result
#'
#' @param x Numeric vector.
#' @param result A `scanr_result` object.
#' @inheritParams plot_time_series
#' @return Invisibly returns the plotted data and mapped change points.
#' @export
plot_change_points <- function(x,
                               result,
                               true_change_points = NULL,
                               index = NULL,
                               x_label = "Time",
                               y_label = "Series",
                               title = NULL,
                               ...) {
  if (is.null(result$change_points)) {
    stop("result must contain change_points", call. = FALSE)
  }

  plot_time_series(
    x = x,
    change_points = result$change_points,
    true_change_points = true_change_points,
    index = index,
    x_label = x_label,
    y_label = y_label,
    title = title %||% "Detected change points",
    ...
  )
}

#' Plot the SWAL/Wasserstein localization curve for a region
#'
#' @param x Numeric vector.
#' @param start First observation in the region, using R's one-based indexing.
#' @param end Last observation in the region, inclusive.
#' @inheritParams plot_time_series
#' @return Invisibly returns the curve data and localized change point.
#' @export
plot_swal_curve <- function(x,
                            start,
                            end,
                            x_label = "Time series",
                            y_label = "Scaled Wasserstein statistic",
                            title = NULL,
                            ...) {
  y <- .plot_series_data(x)$value
  start <- as.integer(start)
  end <- as.integer(end)

  if (is.na(start) || is.na(end) || start < 1L || end > length(y) || end - start + 1L < 3L) {
    stop("start/end must define a valid region with at least 3 observations", call. = FALSE)
  }

  region <- y[start:end]
  refined <- refine_wasserstein(region)
  stats <- refined$statistics
  splits <- start + seq_along(stats) - 1L
  keep <- is.finite(stats)
  df <- data.frame(split = splits[keep], score = stats[keep])
  cp <- start + refined$change_point - 1L

  plot(df$split, df$score,
    type = "l", col = .scanr_blue, lwd = 2,
    xlab = x_label, ylab = y_label, main = title %||% "", ...
  )
  .plot_scan_style()
  abline(v = cp, col = .scanr_orange, lty = "dashed", lwd = 2)

  invisible(list(data = df, change_point = cp))
}

#' Plot retained change-point count by voting threshold
#'
#' @param result A `scanr_result` object.
#' @inheritParams plot_time_series
#' @return Invisibly returns the scree data.
#' @export
plot_vote_scree <- function(result,
                            x_label = "Voting threshold",
                            y_label = "Number of retained change points",
                            title = NULL,
                            ...) {
  scores <- as.numeric(result$scores)
  thresholds <- seq(0, 1, by = 0.01)
  counts <- vapply(thresholds, function(threshold) sum(scores >= threshold), integer(1))
  df <- data.frame(vote_threshold = thresholds, n_change_points = counts)
  selected <- as.numeric(result$parameters$vote_threshold %||% 0.5)

  plot(df$vote_threshold, df$n_change_points,
    type = "b", pch = 16, col = .scanr_blue, lwd = 2,
    xlab = x_label, ylab = y_label, main = title %||% "", ...
  )
  .plot_scan_style()
  abline(v = selected, col = .scanr_orange, lty = "dashed", lwd = 2)

  invisible(df)
}

#' Plot ensemble vote counts for candidate change points
#'
#' @param result A `scanr_result` object.
#' @param x_label_angle Rotation angle for x-axis labels.
#' @inheritParams plot_time_series
#' @return Invisibly returns the vote data.
#' @export
plot_window_votes <- function(result,
                              x_label_angle = 45,
                              x_label = "Candidate change point",
                              y_label = "Window votes",
                              title = NULL,
                              ...) {
  votes <- result$votes
  scores <- result$scores
  n_windows <- max(1L, length(result$window_results))
  vote_threshold <- as.numeric(result$parameters$vote_threshold %||% 0.5)
  threshold_votes <- vote_threshold * n_windows

  df <- data.frame(
    change_point = as.integer(names(votes)),
    votes = as.integer(votes),
    score = as.numeric(scores[names(votes)]),
    stringsAsFactors = FALSE
  )
  df <- df[order(df$change_point), , drop = FALSE]
  above <- df$votes >= threshold_votes
  colors <- ifelse(above, .scanr_orange, .scanr_blue)

  old_par <- par(no.readonly = TRUE)
  on.exit(par(old_par), add = TRUE)
  if (x_label_angle != 0) {
    par(mar = old_par$mar + c(2, 0, 0, 0))
  }

  mids <- barplot(df$votes,
    names.arg = df$change_point,
    col = colors,
    border = NA,
    las = if (x_label_angle == 0) 1 else 2,
    xlab = x_label,
    ylab = y_label,
    main = title %||% "",
    ...
  )
  abline(h = threshold_votes, col = "black", lty = "dashed", lwd = 2)
  legend(
    "topright",
    legend = c("Below threshold", "At or above threshold"),
    fill = c(.scanr_blue, .scanr_orange),
    bty = "n"
  )

  invisible(cbind(df, x = mids))
}

#' Plot scan statistics and bootstrap thresholds
#'
#' @param result A `scanr_result` object.
#' @param window_size Optional window size to plot. Defaults to the first
#'   available window.
#' @inheritParams plot_time_series
#' @return Invisibly returns the threshold data.
#' @export
plot_thresholds <- function(result,
                            window_size = NULL,
                            x_label = "Window start",
                            y_label = "Statistic",
                            title = NULL,
                            ...) {
  if (!length(result$thresholds)) {
    stop("result does not contain threshold diagnostics; rerun with return_all = TRUE", call. = FALSE)
  }

  window_names <- names(result$thresholds)
  selected <- as.character(window_size %||% window_names[[1L]])
  if (!selected %in% window_names) {
    stop("window_size is not available in result$thresholds", call. = FALSE)
  }

  info <- result$thresholds[[selected]]
  df <- data.frame(
    start = info$starts,
    statistic = info$statistics,
    threshold = info$tapered_block_bootstrap_threshold
  )

  ylim <- range(c(df$statistic, df$threshold), finite = TRUE)
  plot(df$start, df$statistic,
    type = "l", col = .scanr_blue, lwd = 2,
    ylim = ylim, xlab = x_label, ylab = y_label,
    main = title %||% paste("Window", selected), ...
  )
  lines(df$start, df$threshold, col = .scanr_orange, lwd = 2, lty = "dashed")
  .plot_scan_style()
  legend(
    "topright",
    legend = c("Statistic", "Bootstrap threshold"),
    col = c(.scanr_blue, .scanr_orange),
    lty = c("solid", "dashed"),
    lwd = 2,
    bty = "n"
  )

  invisible(df)
}
