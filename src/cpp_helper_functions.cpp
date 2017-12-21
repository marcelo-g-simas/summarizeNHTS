#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
double Rcpp_wgtavg(NumericVector x, NumericVector wgts) {
  return sum(x * wgts) / sum(wgts);
}

// [[Rcpp::export]]
double Rcpp_wgtsum(NumericVector x, NumericVector wgts) {
  return sum(x * wgts);
}

//[[Rcpp::export]]
double Rcpp_wgtmed(NumericVector x, NumericVector wgts) {
  //initialize pair vector
  int n = x.size();
  std::vector<std::pair <double, double> > vals(n);
  //push x and wgts into a vector of pairs
  for(int i = 0; i < n; i++) {
    vals[i] = std::make_pair<double, double>(x[i], wgts[i]);
  }
  //sort values by first element in pair
  std::sort(vals.begin(), vals.end());
  // Calculate half of the total weight sum
  double half_tot_wgt = sum(wgts) / 2;
  // Find the value i that splits the cumulative weight sum
  int i = 0;
  double wgt_sum = vals[i].second;
  while(wgt_sum < half_tot_wgt) {
    i++;
    wgt_sum += vals[i].second;
  }
  // Initialize output
  double out;
  // If weight is split evenly, find the avg of the split
  if( wgt_sum == half_tot_wgt ) {
    out = (vals[i].first + vals[i + 1].first) / 2;
  } else {
    out = vals[i].first;
  }
  return out;
}
