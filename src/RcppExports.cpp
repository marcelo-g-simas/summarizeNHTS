// Generated by using Rcpp::compileAttributes() -> do not edit by hand
// Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#include <Rcpp.h>

using namespace Rcpp;

// Rcpp_wgtavg
double Rcpp_wgtavg(NumericVector x, NumericVector wgts);
RcppExport SEXP summarizeNHTS_Rcpp_wgtavg(SEXP xSEXP, SEXP wgtsSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< NumericVector >::type x(xSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type wgts(wgtsSEXP);
    rcpp_result_gen = Rcpp::wrap(Rcpp_wgtavg(x, wgts));
    return rcpp_result_gen;
END_RCPP
}
// Rcpp_sum
double Rcpp_sum(NumericVector x);
RcppExport SEXP summarizeNHTS_Rcpp_sum(SEXP xSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< NumericVector >::type x(xSEXP);
    rcpp_result_gen = Rcpp::wrap(Rcpp_sum(x));
    return rcpp_result_gen;
END_RCPP
}

static const R_CallMethodDef CallEntries[] = {
    {"summarizeNHTS_Rcpp_wgtavg", (DL_FUNC) &summarizeNHTS_Rcpp_wgtavg, 2},
    {"summarizeNHTS_Rcpp_sum", (DL_FUNC) &summarizeNHTS_Rcpp_sum, 1},
    {NULL, NULL, 0}
};

RcppExport void R_init_summarizeNHTS(DllInfo *dll) {
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}
