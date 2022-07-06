wn_n_params = function() {
  1
}

wn_param_names = function() {
  c("sigma2")
}

wn_v_tran = function(theta) {
  theta_t = rep(0, length(theta))
  theta_t[1] = transfo_exp(theta[1])
  return(theta_t)
}

wn_v_tran_inv = function(theta) {
  theta_t = rep(0, length(theta))
  theta_t[1] = inv_transfo_exp(theta[1])
  return(theta_t)
}

#' @importFrom wv wn_to_wv
wn_theo_wv = function(theta, scales) {
  wn_to_wv(theta[1], scales)
}

wn_autocovariance = function(theta, n) {
  acf = rep(0, n)
  acf[1] = theta[1]
  acf
}


wn_theta_to_exp =  function(theta) {
  bquote( WN(sigma^2 == .(sprintf('%.2f', theta[1]))) )
}

wn_pick_params_hector = function(json) {
  params = c(
    json$driving_noise^2 * json$NoiseModel$White$fraction
  )
}

wn_name_hector = function() {
  "White"
}