#include <Rcpp.h>
#include <numeric>
using namespace Rcpp;

//' Compute the power law autocovariance vector
//' @param theta A \code{vector} of length 2 specifying the parameters of the powerlaw process
//' @param n A \code{scalar} specifying the length of the signal
// [[Rcpp::export]]
NumericVector powerlaw_autocovariance(const NumericVector theta, const int n) {
  double sigma2 = theta(0);
  double d = theta(1);
  NumericVector acf (n);
  acf(0) = ::tgamma(1.0-2.0*d) / pow(::tgamma(1.0-d), 2)*sigma2;
  for (int i=1; i < n; ++i) {
    acf(i) = (d+i-1.0)*acf(i-1)/(i-d);
  }
  return(acf);
}