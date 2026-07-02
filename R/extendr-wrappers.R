# Generated-style wrappers for the Rust functions exported by extendr.

.scan_detector_json <- function(series, window_sizes, n_boot, alpha, seed,
                                tolerance, workers, backend, change_type, eps,
                                block_length, taper_ratio, center, batch_size) {
  .Call(wrap__scan_detector_json, series, window_sizes, n_boot, alpha, seed,
        tolerance, workers, backend, change_type, eps, block_length,
        taper_ratio, center, batch_size)
}

.scan_single_window_json <- function(series, window_size, n_boot, alpha, seed,
                                     change_type, eps, block_length,
                                     taper_ratio, center, batch_size) {
  .Call(wrap__scan_single_window_json, series, window_size, n_boot, alpha,
        seed, change_type, eps, block_length, taper_ratio, center, batch_size)
}

.refine_cusum_json <- function(series) {
  .Call(wrap__refine_cusum_json, series)
}

.refine_wasserstein_json <- function(series) {
  .Call(wrap__refine_wasserstein_json, series)
}

.swal_statistic_json <- function(series, change_type) {
  .Call(wrap__swal_statistic_json, series, change_type)
}

.wasserstein_statistic_json <- function(left, right) {
  .Call(wrap__wasserstein_statistic_json, left, right)
}

.ipm_statistic_json <- function(left, right) {
  .Call(wrap__ipm_statistic_json, left, right)
}
