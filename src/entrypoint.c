#include <Rinternals.h>
#include <R_ext/Rdynload.h>

extern SEXP wrap__scan_detector_json(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP wrap__scan_single_window_json(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP wrap__refine_cusum_json(SEXP);
extern SEXP wrap__refine_wasserstein_json(SEXP);
extern SEXP wrap__swal_statistic_json(SEXP, SEXP);
extern SEXP wrap__wasserstein_statistic_json(SEXP, SEXP);
extern SEXP wrap__ipm_statistic_json(SEXP, SEXP);

static const R_CallMethodDef CallEntries[] = {
    {"wrap__scan_detector_json", (DL_FUNC) &wrap__scan_detector_json, 14},
    {"wrap__scan_single_window_json", (DL_FUNC) &wrap__scan_single_window_json, 11},
    {"wrap__refine_cusum_json", (DL_FUNC) &wrap__refine_cusum_json, 1},
    {"wrap__refine_wasserstein_json", (DL_FUNC) &wrap__refine_wasserstein_json, 1},
    {"wrap__swal_statistic_json", (DL_FUNC) &wrap__swal_statistic_json, 2},
    {"wrap__wasserstein_statistic_json", (DL_FUNC) &wrap__wasserstein_statistic_json, 2},
    {"wrap__ipm_statistic_json", (DL_FUNC) &wrap__ipm_statistic_json, 2},
    {NULL, NULL, 0}
};

void R_init_Scanr(DllInfo *dll) {
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}
