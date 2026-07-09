.scanr_blue <- "#00008B"
.scanr_orange <- "#f5710a"
.scanr_grid <- "#D9D9D9"

#' @importFrom rlang .data
NULL

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

.scanr_theme <- function() {
  ggplot2::theme_minimal() +
    ggplot2::theme(
      panel.grid.major = ggplot2::element_line(color = .scanr_grid, linetype = "dotted"),
      panel.grid.minor = ggplot2::element_blank(),
      plot.title = ggplot2::element_text(face = "bold"),
      legend.title = ggplot2::element_blank()
    )
}

.attach_plot_data <- function(plot, ...) {
  attr(plot, "scanr_data") <- list(...)
  plot
}

#' Visualize a time series with optional change-point markers
#'
#' @param x Numeric vector.
#' @param change_points Optional detected change points.
#' @param true_change_points Optional true change points.
#' @param index Optional x-axis index with the same length as `x`.
#' @param x_label X-axis label.
#' @param y_label Y-axis label.
#' @param title Plot title.
#' @param ... Reserved for future plot options.
#' @return A `ggplot` object.
#' @export
vis_time_series <- function(x,
                            change_points = NULL,
                            true_change_points = NULL,
                            index = NULL,
                            x_label = "Time",
                            y_label = "Value",
                            title = NULL,
                            ...) {
  df <- .plot_series_data(x, index = index)
  detected <- .map_cps_to_axis(change_points, df$t)
  truth <- .map_cps_to_axis(true_change_points, df$t)

  p <- ggplot2::ggplot(df, ggplot2::aes(x = .data$t, y = .data$value)) +
    ggplot2::geom_line(color = .scanr_blue, linewidth = 0.8) +
    ggplot2::labs(x = x_label, y = y_label, title = title) +
    .scanr_theme()

  if (length(detected)) {
    p <- p +
      ggplot2::geom_vline(
        ggplot2::aes(
          xintercept = .data$x,
          color = "Detected change points",
          linetype = "Detected change points"
        ),
        data = data.frame(x = detected),
        linewidth = 0.7
      )
  }
  if (length(truth)) {
    p <- p +
      ggplot2::geom_vline(
        ggplot2::aes(
          xintercept = .data$x,
          color = "True change points",
          linetype = "True change points"
        ),
        data = data.frame(x = truth),
        linewidth = 0.7
      )
  }

  p <- p +
    ggplot2::scale_color_manual(
      name = NULL,
      values = c("Detected change points" = .scanr_orange, "True change points" = "black"),
      drop = TRUE
    ) +
    ggplot2::scale_linetype_manual(
      name = NULL,
      values = c("Detected change points" = "solid", "True change points" = "dotted"),
      drop = TRUE
    ) +
    ggplot2::guides(
      color = ggplot2::guide_legend(nrow = 1),
      linetype = ggplot2::guide_legend(nrow = 1)
    ) +
    ggplot2::theme(
      legend.position = "bottom",
      legend.direction = "horizontal"
    )

  .attach_plot_data(p, data = df, detected = detected, truth = truth)
}

#' Visualize detected change points from a scan result
#'
#' @param x Numeric vector.
#' @param result A `scanr_result` object.
#' @inheritParams vis_time_series
#' @return A `ggplot` object.
#' @export
vis_change_points <- function(x,
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

  vis_time_series(
    x = x,
    change_points = result$change_points,
    true_change_points = true_change_points,
    index = index,
    x_label = x_label,
    y_label = y_label,
    title = title %||% "",
    ...
  )
}

#' Visualize the SWAL/Wasserstein localization curve for a region
#'
#' @param x Numeric vector.
#' @param start First observation in the region, using R's one-based indexing.
#' @param end Last observation in the region, inclusive.
#' @inheritParams vis_time_series
#' @return A `ggplot` object.
#' @export
vis_swal_curve <- function(x,
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
  refined <- ts_wasserstein(region)
  stats <- refined$statistics
  splits <- start + seq_along(stats) - 1L
  keep <- is.finite(stats)
  df <- data.frame(split = splits[keep], score = stats[keep])
  cp <- start + refined$change_point - 1L

  p <- ggplot2::ggplot(df, ggplot2::aes(x = .data$split, y = .data$score)) +
    ggplot2::geom_line(color = .scanr_blue, linewidth = 0.8) +
    ggplot2::geom_vline(xintercept = cp, color = .scanr_orange, linetype = "dashed", linewidth = 0.7) +
    ggplot2::labs(x = x_label, y = y_label, title = title %||% "") +
    .scanr_theme()

  .attach_plot_data(p, data = df, change_point = cp)
}

#' Visualize retained change-point count by voting threshold
#'
#' @param result A `scanr_result` object.
#' @inheritParams vis_time_series
#' @return A `ggplot` object.
#' @export
vis_vote_scree <- function(result,
                           x_label = "Voting threshold",
                           y_label = "Number of retained change points",
                           title = NULL,
                           ...) {
  scores <- as.numeric(result$scores)
  thresholds <- seq(0, 1, by = 0.01)
  counts <- vapply(thresholds, function(threshold) sum(scores >= threshold), integer(1))
  df <- data.frame(vote_threshold = thresholds, n_change_points = counts)
  selected <- as.numeric(result$parameters$vote_threshold %||% 0.5)

  p <- ggplot2::ggplot(df, ggplot2::aes(x = .data$vote_threshold, y = .data$n_change_points)) +
    ggplot2::geom_line(color = .scanr_blue, linewidth = 0.8) +
    ggplot2::geom_point(color = .scanr_blue, size = 1.4) +
    ggplot2::geom_vline(xintercept = selected, color = .scanr_orange, linetype = "dashed", linewidth = 0.7) +
    ggplot2::labs(x = x_label, y = y_label, title = title %||% "") +
    .scanr_theme()

  .attach_plot_data(p, data = df)
}

#' Visualize ensemble vote counts for candidate change points
#'
#' @param result A `scanr_result` object.
#' @param x_label_angle Rotation angle for x-axis labels.
#' @inheritParams vis_time_series
#' @return A `ggplot` object.
#' @export
vis_window_votes <- function(result,
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
  df$status <- ifelse(df$votes >= threshold_votes, "At or above threshold", "Below threshold")
  df$change_point_label <- factor(df$change_point, levels = df$change_point)

  p <- ggplot2::ggplot(df, ggplot2::aes(x = .data$change_point_label, y = .data$votes, fill = .data$status)) +
    ggplot2::geom_col(width = 0.8) +
    ggplot2::geom_hline(yintercept = threshold_votes, linetype = "dashed", linewidth = 0.7) +
    ggplot2::scale_fill_manual(
      values = c("Below threshold" = .scanr_blue, "At or above threshold" = .scanr_orange)
    ) +
    ggplot2::labs(x = x_label, y = y_label, title = title %||% "") +
    .scanr_theme() +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = x_label_angle, hjust = 1))

  .attach_plot_data(p, data = df)
}

#' Visualize scan statistics and bootstrap thresholds
#'
#' @param result A `scanr_result` object.
#' @param window_size Optional window size to plot. Defaults to the first
#'   available window.
#' @inheritParams vis_time_series
#' @return A `ggplot` object.
#' @export
vis_thresholds <- function(result,
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
    start = rep(info$starts, 2L),
    value = c(info$statistics, info$tapered_block_bootstrap_threshold),
    series = rep(c("Statistic", "Bootstrap threshold"), each = length(info$starts))
  )

  p <- ggplot2::ggplot(df, ggplot2::aes(x = .data$start, y = .data$value, color = .data$series, linetype = .data$series)) +
    ggplot2::geom_line(linewidth = 0.8) +
    ggplot2::scale_color_manual(values = c("Statistic" = .scanr_blue, "Bootstrap threshold" = .scanr_orange)) +
    ggplot2::scale_linetype_manual(values = c("Statistic" = "solid", "Bootstrap threshold" = "dashed")) +
    ggplot2::labs(x = x_label, y = y_label, title = title %||% paste("Window", selected)) +
    .scanr_theme()

  .attach_plot_data(p, data = df)
}
