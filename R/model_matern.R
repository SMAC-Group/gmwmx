matern_n_params = function() {
  3
}

matern_param_names = function() {
  c("sigma^2", "lambda", "alpha")
}

matern_v_tran = function(theta) {
  theta_t = rep(0, length(theta))
  
  theta_t[1:2] = transfo_exp(theta[1:2]) # sigma2 lambda
  theta_t[3] = transfo_logit(theta[3])*1.5 # lambda
  
  return(theta_t)
}

matern_v_tran_inv = function(theta) {
  theta_t = rep(0, length(theta))
  
  theta_t[1:2] = inv_transfo_exp(theta[1:2]) # sigma2 lambda
  theta_t[3] = inv_transfo_logit(theta[3]/1.5)
  
  return(theta_t)
}

matern_theo_wv = function(theta, scales) {
  autocovariance_to_wv(matern_autocovariance(theta, tail(scales,1)), scales)
}

Ma <- function(x, alpha){
  2/gamma(alpha-1/2)/2^(alpha-1/2)*abs(x)^(alpha-1/2)*besselK(abs(x), abs(alpha-1/2))
}

matern_autocovariance = function(theta, n) {
  sigma2 = theta[1]
  lambda = theta[2]
  alpha = theta[3]
  
  acf = c(sigma2, sigma2*Ma(lambda* (1:(n-1)), alpha = alpha))
  return(acf)
}

matern_theta_to_exp =  function(theta) {
  bquote( Matern(sigma^2 == .(sprintf('%.2f', theta[1])), lambda == .(sprintf('%.3f', theta[2])), alpha == .(sprintf('%.3f', theta[3]))  ) )
}

matern_pick_params_hector = function(json) {
  params = c(
    json$driving_noise^2 * json$NoiseModel$Matern$fraction,
    json$NoiseModel$Matern$lambda,
    2*json$NoiseModel$Matern$d
  )
}

matern_name_hector = function() {
  "Matern"
}