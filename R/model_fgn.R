

fgn_n_params = function() {
  2
}

fgn_param_names = function() {
  c("sigma2", "H")
}

fgn_v_tran = function(theta) {
  theta_t = rep(0, length(theta))
  
  theta_t[1] = transfo_exp(theta[1]) # sigma2
  theta_t[2] = transfo_logit(theta[2]) # H
  
  return(theta_t)
}

fgn_v_tran_inv = function(theta) {
  theta_t = rep(0, length(theta))
  
  theta_t[1] = inv_transfo_exp(theta[1]) # sigma2
  theta_t[2] = inv_transfo_logit(theta[2]) # H

  return(theta_t)
}

fgn_theo_wv = function(theta, scales) {
  sigma2 = theta[1]
  H = theta[2]
  sigma2*(1 - 2^(2*H - 2))*(scales/2)^(2*H - 2)
}

#' FGN autocovariance
#' 
#' @param theta A \code{vector} of length 2 specifying the parameters of the FGN
#' @param n A \code{scalar} specifying the length of the signal
#' @importFrom longmemo ckFGN0
fgn_autocovariance = function(theta, n) {
  sigma2 = theta[1]
  H = theta[2]
  
  acf = sigma2*ckFGN0(n = n, H = H)
}

fgn_theta_to_exp =  function(theta) {
  bquote( fGn(sigma^2 == .(sprintf('%.2f', theta[1])), H == .(sprintf('%.2f', theta[2]))) )
}