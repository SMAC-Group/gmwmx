#include <numeric>
#include <RcppArmadillo.h>
using namespace Rcpp;



// Compute Sigma matrix given an acf vector
// 
// @param acf A \code{vector} specifying the autocovariance vector.
// [[Rcpp::export]]
arma::mat acf_to_sigma(const arma::vec &acf){
  // Initialize matrix
  arma::mat Sigma = arma::toeplitz(acf);
  return(Sigma);

}



