rw_n_params = function() {
  1
}

rw_param_names = function() {
  c("sigma2")
}

rw_v_tran = function(theta) {
  theta_t = rep(0, length(theta))
  theta_t[1] = transfo_exp(theta[1])
  return(theta_t)
}

rw_v_tran_inv = function(theta) {
  theta_t = rep(0, length(theta))
  theta_t[1] = inv_transfo_exp(theta[1])
  return(theta_t)
}

#' @importFrom wv rw_to_wv
rw_theo_wv = function(theta, scales) {
  rw_to_wv(theta[1], scales)
}


#  this needs to be implemented differently, create the sigma matrix for this process independently from the autocorrelation vector. 
rw_autocovariance = function(theta, n) {
  acf = seq(1,n)*theta
  acf
}

rw_theta_to_exp =  function(theta) {
  bquote( RW(gamma^2 == .(sprintf('%.2f', theta[1]))) )
}

rw_pick_params_hector = function(json) {
  params = c(
    json$driving_noise^2 * json$NoiseModel$RandomWalkGGM$fraction
  )
}

rw_name_hector = function() {
  "RandomWalkGGM"
}
