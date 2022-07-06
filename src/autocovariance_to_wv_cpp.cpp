#include <Rcpp.h>
#include <numeric>
using namespace Rcpp;

//  Subset range of a vector
// 
Rcpp::NumericVector subset_range(Rcpp::NumericVector x,
                                 int start = 1, int end = 100) {
  
  // Use the Range function to create a positional index sequence
  return x[Rcpp::Range(start, end)];
}


//  Vectorized exponentiation
// 
NumericVector compute_power_of_a_base(int x, int J){
  IntegerVector power = seq(0, J);
  NumericVector out (power.length());
  for (int i=0; i < power.length(); ++i) {
    out(i) = pow(x, power(i));
  }
  return(out);
}



// Compute the theoretical WV given the autocovariance vector 
// 
// @param acf A \code{vector} specifying the autocovariance vector
// @param tau A \code{vector} of Wavelet Variance scales
// [[Rcpp::export]]
NumericVector autocovariance_to_wv(const NumericVector acf, const NumericVector tau) {
  // compute max scale
  NumericVector J = log10(tail(tau, 1))/log10(2);
  // Index first element of acf
  double var_process = acf(0);
  NumericVector autocorr = subset_range(acf, 1, acf.length()-1) / var_process ;
  NumericVector ms = compute_power_of_a_base(2, J(0)-1);
  //  Initialize vector for theoretical wavelet variance
  NumericVector theo_wv (J(0));
  
  for (int j=1; j < J(0)+1; ++j) {
    double m = ms(j-1);
    double inter = m * (1- autocorr(m-1));
    if(m>1){
      for (int i=1; i <= m-1; ++i) {
        inter = inter + i*(2*autocorr[m - i-1] - autocorr[i-1] - autocorr[2*m-i-1]);
      }
    }
    theo_wv[j-1] = inter / pow(m, 2) * var_process / 2 ;
  }
  return(theo_wv);
}




