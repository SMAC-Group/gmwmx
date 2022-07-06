# this has been converted to in RCPP in src/autocovariance_to_wv_cpp.cpp 

# autocovariance_to_wv <- function(acf, tau) {
#   
#   J = log2(tail(tau,1))
#   
#   var_process = acf[1]
#   autocorr = acf[-1]/var_process # tau = 0 is not evaluated
#   
#   ms = 2^(0:(J-1))
#   theo_wv = rep(NA, J)
#   
#   for (j in 1:J){
#     m = ms[j]
#     inter = m*(1 - autocorr[m])
#     
#     if (m > 1){
#       for (i in 1:(m-1)){
#         inter = inter + i*(2*autocorr[m - i] - autocorr[i] - autocorr[2*m-i])
#       }
#     }
#     
#     theo_wv[j] = inter/m^2*var_process/2
#   }
#   
#   theo_wv
# }