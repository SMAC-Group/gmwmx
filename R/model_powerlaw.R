powerlaw_n_params = function() {
  2
}

powerlaw_param_names = function() {
  c("sigma2", "d")
}

powerlaw_v_tran = function(theta) {
  theta_t = rep(0, length(theta))
  
  theta_t[1] = transfo_exp(theta[1]) # sigma2
  theta_t[2] = transfo_logit(theta[2])*0.4999 # fgn d (from 0 to 0.5)

  return(theta_t)
}

powerlaw_v_tran_inv = function(theta) {
  theta_t = rep(0, length(theta))
  
  theta_t[1] = inv_transfo_exp(theta[1]) # sigma2
  theta_t[2] = inv_transfo_logit(theta[2]/0.4999) # fgn d (from 0 to 0.5)

  return(theta_t)
}

powerlaw_theo_wv = function(theta, scales) {
  autocovariance_to_wv(powerlaw_autocovariance(theta, tail(scales,1)), scales)
}

# powerlaw_autocovariance = function(theta, n) {
#   sigma2 = theta[1]
#   d = theta[2]
#   
#   acf = rep(0, n)
#   acf[1] = gamma(1.0-2.0*d)/gamma(1.0-d)^2*sigma2
#   
#   for (j in 1:(n-1)) {
#     acf[j+1] = (d+j-1.0)*acf[j]/(j-d);
#   }
#   
#   acf
# }

powerlaw_theta_to_exp =  function(theta) {
  bquote( PowerLaw(sigma^2 == .(sprintf('%.2f', theta[1])), d == .(sprintf('%.2f', theta[2])) ) )
}

powerlaw_pick_params_hector = function(json) {
  params = c(
    json$driving_noise^2 * json$NoiseModel$Powerlaw$fraction,
    json$NoiseModel$Powerlaw$d
  )
}

powerlaw_name_hector = function() {
  "Powerlaw"
}