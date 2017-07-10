#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
double Rcpp_wgtavg(NumericVector x, NumericVector wgts) {
  return sum(x * wgts) / sum(wgts);
}

// [[Rcpp::export]]
double Rcpp_sum(NumericVector x) {
  return sum(x);
}
