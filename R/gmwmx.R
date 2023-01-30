optim_wv = function(theta, nu_hat, model){
  scales = nu_hat$scales
  
  # apply variable transformation
  theta_t = gen_v_tran(theta, model)
  
  # compute theoretical wavelet variances
  nu_theo = gen_theo_wv(theta_t, scales, model)
  
  # define cost
  omega = diag(1 / (nu_hat$ci_low - nu_hat$ci_high)^2)
  
  t(nu_hat$variance - nu_theo) %*% omega %*% (nu_hat$variance - nu_theo)
}


fit_base = function(theta_0, wv_emph, model_string, maxit=1e6, method=NULL) {
  
  model = create_model_descriptor(model_string)
  
  if (length(theta_0) != model$n_params) {
    stop(sprintf("model %s has %d params, given %d", model_string, model$n_params, length(theta_0)))
  }
  
  res = optim(par = gen_v_tran_inv(theta_0, model),
              fn = optim_wv, 
              nu_hat = wv_emph, 
              model = model,
              control = list(maxit=maxit), method=method)
  
  theta_hat = gen_v_tran(res$par, model)
  
  names(theta_hat) = gen_param_names(model)
  
  theta_hat
}







#' Define matrix A of the functional model
#' 
#' @param t_nogap A \code{vector} specifying the index of the time series.
#' @param jumps A \code{vector} specifying the time at which there is a mean shift of the time series. Should be specified to \code{NULL} if there is not presence of offsets in the signal.
#' @param n_seasonal An \code{integer} specifying the number of sinusoidal signals in the time series.
#' @export 
#' @examples
#' n= 10*365
#' jump_vec <- c(200, 300, 500)
#' nbr_sin = 2
#' A <- create_A_matrix(1:n, jump_vec, n_seasonal = nbr_sin)
#' head(A)
#' A <- create_A_matrix(1:n, jumps = NULL, n_seasonal = nbr_sin)
#' head(A)
#' @return Matrix A in order to compute the functional component of the model in a linear fashion 
create_A_matrix = function(t_nogap, jumps, n_seasonal) {
  
  # beta (bias, trend, A*cos(U) sin1, A*sin(U) sin1, A*cos(U) sin2, A*sin(U) sin2, ..., height_jump1, height_jump2, ... )
  
  X = matrix(0, nrow = length(t_nogap), ncol = 2+2*n_seasonal + length(jumps))
  
  X[,1] = 1
  X[,2] = t_nogap - 0.5*(t_nogap[1]+tail(t_nogap,1)) # reference is the middle of the time series
  
  if (n_seasonal > 0) {
    for (i in 1:n_seasonal) {
      X[,2+(i-1)*2+1] = sin((t_nogap-51544.0)*i*2*pi/365.25) # time is shifted as done in Hector DesignMatrix.cpp:388
      X[,2+(i-1)*2+2] = cos((t_nogap-51544.0)*i*2*pi/365.25)
    }
  }
  
  if (!is.null(jumps[1])) {
    for (i in 1:length(jumps)) {
      it = min(which(t_nogap > jumps[i]-1e-6))
      
      X[,2+2*n_seasonal+i] = c(rep(0, it-1), rep(1, length(t_nogap)-it+1))
    }
  }
  
  X
}



powmat_non_int = function(mat, power){
  with(eigen(mat), vectors %*% (values^power * t(vectors))) 
}

wvar_missing = function(Xt, alpha = 0.05){
  wv = wv::wvar(Xt, alpha = alpha)
  J = length(wv$variance)
  # wv=list("variance"=NULL,
  #         "ci_low" = NULL,
  #         "ci_high"=NULL,
  #         "robust" =F,
  #         "eff"=0.6,
  #         "alpha"=alpha,
  #         "scales"=NULL,
  #         "decomp" = "modwt",
  #         "unit"=NULL,
  #         "filter" = "haar")
  # class(wv) = "wvar"
  # J = floor(log2(length(Xt))-1)
  modwt_x = wv::modwt(Xt)
  nu = nu_down = nu_up = rep(NA, J)
  alpha_ov_2 = alpha/2
  for(j in 1:J){
    Wt = na.omit(modwt_x[[j]])
    nu[j] = mean(Wt^2)
    Mj = length(Wt)
    eta3 = max(Mj/2^j, 1)
    # There is a small bug with the CI (to be fixed later)
    nu_up[j] = eta3*nu[j] / qchisq(1-alpha_ov_2, eta3, 1, 0)
    nu_down[j] = eta3*nu[j] / qchisq(alpha_ov_2, eta3, 1, 0)
  }
  
  wv$variance = nu
  wv$ci_low = nu_down
  wv$ci_high = nu_up
  
  Jmax = length(na.omit(wv$variance))
  wv$variance = wv$variance[1:Jmax]
  wv$ci_low = wv$ci_low[1:Jmax]
  wv$ci_high = wv$ci_high[1:Jmax]
  wv$scales = wv$scales[1:Jmax,1]
  wv
  
}


#' Estimate a stochastic model in a two-steps procedure using the GMWMX estimator.
#' 
#' @param x A \code{gnssts} object
#' @param theta_0 A \code{vector} specifying the initial values for the vector of parameter of the stochastic model considered.
#' @param n_seasonal An \code{integer} specifying the number of seasonal component in the time series.
#' @param model_string A \code{string} specifying the model to be estimated.
#' @param method A \code{string} specifying the numerical optimization method that should be supplied to \code{optim()} 
#' @param maxit An \code{integer} specifying the maximum number of iterations for the numerical optimization procedure.
#' @param ci A \code{boolean} specifying if confidence intervals for the estimated parameters should be computed. 
#' @param k_iter An \code{integer} specifying the number of time the two steps GMWMX procedure should be run. 
#' @return A \code{gnsstsmodel} object.
#' @importFrom Matrix solve
#' @importFrom  stats coefficients lm optim residuals 
#' @importFrom wv wvar modwt
#' @importFrom ltsa TrenchInverse
#' @importFrom stats na.omit qchisq

#' @export
#' 
#' @examples 
#' \dontrun{
#' data(cola)
#' fit_gmwmx = estimate_gmwmx(x = cola,
#'                            theta_0 = c(0.1,0.1,0.1,0.1), 
#'                            n_seasonal = 1, 
#'                            model_string = "wn+matern")
#' }
#' 
estimate_gmwmx <- function(
  x,
  theta_0, 
  n_seasonal = 1, 
  model_string,
  method = "L-BFGS-B",
  maxit = 1e6,
  ci = FALSE,
  k_iter = 1
  ) {
  

  
  
 
  # check that the provided object is a gnssts object
  if (!("gnssts" %in% class(x))) {
    stop("x must be an object of type 'gnssts'")
  }
  
  # check that k_iter is a numeric value with value either 1 or 2
  if(! k_iter %in% c(1,2) || !is.numeric(k_iter)){
    stop("Incorrect provided argument k_iter. Argument k_iter should be either the numeric value 1 or 2. Default value if 1.")
  }
  
  # throw a warning about random walk with gmwmx
  if(grepl(pattern = "rw", x = model_string)){
    warning("The properties of the GMWMX when the specified stochastic model includes a Random Walk have not been studied and the results may be unreliable. Moreover, the GMWMX-2 and confidence intervals are not yet supported.")
  }
  
  # check that GMWMX2 or ci are not specified if rw in stochastic model
  if(grepl(pattern = "rw", x = model_string)){
    if(k_iter>1){
      stop("Random Walk specified in stochastic model. The GMWMX-2 is not yet implemented when the specified stochastic model includes a Random Walk.")
    }
    if(ci){
      stop("Random Walk specified in stochastic model. Confidence intervals are not yet implemented when the specified stochastic model includes a Random Walk.")
    }
  }
  
  # create model
  model = create_model_descriptor(model_string)
  
  # handle gaps
  t_nogap = x$t[1]:tail(x$t,1) # TODO: handle sampling period
  which_data = is.element(t_nogap, x$t)
  
  # gmwmx algorithm 
  timing = system.time({
  
    X = create_A_matrix(t_nogap = t_nogap, jumps = x$jumps, n_seasonal =  n_seasonal)
    
    # create beta_matrix
    beta_mat = matrix(NA , ncol= dim(X)[2], nrow= k_iter)
    
    # fit regression
    sol = lm(x$y ~ X[which_data,-1]) # do the linear regression only where we have data
    beta = coefficients(sol)
    
    # set names to estimated coeffients 
    names(beta) <- c("bias", "trend", rep(c("A*cos(U)", "A*sin(U)"), n_seasonal), rep("jump", length(x$jumps)))
    
    # save beta in first row of beta_mat
    beta_mat[1, ] = beta

    #  compute residuals
    rsd_data = x$y - X[which_data, ] %*% beta # compute residuals where I have data
    
    #  create vector of length of signal including NA
    rsd_with_na = vector(mode = "numeric", length = length(t_nogap))
    
    # set non NA value to their value
    rsd_with_na[which_data] = rsd_data 
    
    # set to NA value which are not set
    rsd_with_na[!which_data] = NA
    
    # compute residuals only where I have data
    rsd = rsd_with_na
    
    # compute wavelet variance of the residuals
    wv_rsd = wvar_missing(rsd)

    # fit the stochastic model on the residuals
    theta_hat = fit_base(
      theta_0 = theta_0, 
      wv_emph = wv_rsd, 
      model_string  = model_string,
      method = method,
      maxit = maxit
    )
    
    # compute variance covariance matrix for weighted least square 
    Sigma = gen_covariance(theta_hat, length(t_nogap), model)
    
    # repeat procedure k-1 times if specified
    if(k_iter > 1){
    
      for(k in seq((k_iter-1))){
        # compute inverse of sigma
        Sigma_inv = ltsa::TrenchInverse(Sigma)
        
        # subset X and Sigma
        X_sub = X[which_data, ]
        
        # subset Sigma
        Sigma_inv_sub = Sigma_inv[which_data,which_data]
        
        # Compute GLS
        beta = Matrix::solve(t(X_sub) %*% Sigma_inv_sub %*%X_sub) %*% t(X_sub)%*%Sigma_inv_sub %*%x$y

        # save beta in beta mat
        beta_mat[k+1, ] = beta
        
        # define name of beta
        # do not consider intercept
        names(beta) <- c("bias", "trend", rep(c("A*cos(U)", "A*sin(U)"), n_seasonal), rep("jump", length(x$jumps)))
        
        # compute residuals
        rsd_data = x$y - X[which_data, ] %*% beta # compute residuals where I have data
        
        #  create vector of length of signal including NA
        rsd_with_na = vector(mode = "numeric", length = length(t_nogap))
        
        # set non NA value to their value
        rsd_with_na[which_data] = rsd_data 
        
        # set to NA value which are not set
        rsd_with_na[!which_data] = NA
        
        # compute residuals only where I have data
        rsd = rsd_with_na
        
        # compute wavelet variance of the residuals
        wv_rsd = wvar_missing(rsd)
        
        # fit the stochastic model on the residuals
        theta_hat = fit_base(
          theta_0 = theta_0, 
          wv_emph = wv_rsd, 
          model_string = model_string,
          method = method,
          maxit = maxit
        )
        
        # compute variance covariance matrix for least square 
        Sigma = gen_covariance(theta_hat, length(t_nogap), model)
        
      }
    }
    
    # if required, compute confidence intervals for functional parameters
    if (ci == TRUE) {
      # variance covariance matrix for least square 
      
      # Sigma = gen_covariance(theta_hat, length(t_nogap), model)
      
      A = Matrix::solve(t(X[which_data,]) %*% X[which_data,]) %*% t(X[which_data,])
      Sigma_beta = A %*% Sigma[which_data, which_data] %*% t(A)
      
      ci_beta = sqrt(diag(Sigma_beta))
      names(ci_beta) <- sprintf("std_%s", names(beta))
    }
  })
  
  ret = list()
  ret$model = model
  
  # functional parameters
  ret$beta_hat = beta
  ret$beta_std = if (ci) ci_beta else rep(NA, length(beta))
  
  # stochastic parameter
  ret$theta_hat = theta_hat
  ret$theta_std = rep(NA, length(theta_hat)) # TODO: CI for theta not computed
  
  # original time series
  ret$input = list()
  ret$input$jumps = x$jumps
  ret$input$t_orig = x$t
  ret$input$x_orig = x$y
  
  # fitted functional model
  ret$output = list()
  ret$output$t_nogap = t_nogap
  ret$output$x_hat = X %*% beta
  
  # residuals
  ret$residuals = rsd
  
  # ww of the residuals
  ret$wv_residuals = wv_rsd
  
  # timing
  ret$estimation_time = timing
  
  # define class of returned object
  class(ret) <- "gnsstsmodel"
  
  # return
  return(ret)
}