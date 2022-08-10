# function to infuse remove outliers template
#' @importFrom stringi stri_replace
infuse_remove_outliers_tmp = function(data_file,
                                      working_folder,
                                      output_file,
                                      n_seasonal,
                                      IQ_factor,
                                      x){
  
  
  remove_outliers_tmpl = "DataFile              data_file_value\nDataDirectory         data_directory_value\nOutputFile            output_file_value\ninterpolate           no\nseasonalsignal        seasonal_signal_value\nhalfseasonalsignal    half_seasonal_signal_value\nestimateoffsets       estimate_offsets_value\nIQ_factor             iq_factor_value\nPhysicalUnit          mm\n"
  
  # replace data_file_value by data_file
  tpl_1 = stringi::stri_replace( str = remove_outliers_tmpl, regex =  "data_file_value" , replacement = data_file)
  
  tpl_2 = stringi::stri_replace( str = tpl_1,regex =  "data_directory_value" , replacement = working_folder)
  
  tpl_3 = stringi::stri_replace( str = tpl_2,regex =  "output_file_value" , replacement = output_file)
  
  tpl_4 = stringi::stri_replace( str = tpl_3,regex =  "seasonal_signal_value" , replacement = if (n_seasonal > 0) "yes" else "no")
  
  tpl_5 = stringi::stri_replace( str = tpl_4,regex =  "half_seasonal_signal_value" , replacement = if (n_seasonal > 1) "yes" else "no")
  
  tpl_6 = stringi::stri_replace( str = tpl_5,regex =  "estimate_offsets_value" , replacement = if (!is.null(x$jumps[1])) "yes" else "no")
  
  tpl_7 = stringi::stri_replace( str = tpl_6,regex =  "iq_factor_value" , replacement = sprintf('%.2f', IQ_factor))
  
  tpl_7
}

# function to infuse estimate trend template
#' @importFrom stringi stri_replace
infuse_estimate_trend_tmp = function(data_file,
                                     working_folder,
                                     output_file,
                                     noise_model,
                                     n_seasonal,
                                     x){
  
  estimate_trend_tmpl = "DataFile              data_file_value\nDataDirectory         data_directory_value\nOutputFile            output_file_value\ninterpolate           no\nPhysicalUnit          mm\nScaleFactor           1.0\nNoiseModels           noise_models_value\nseasonalsignal        seasonal_signal_value\nhalfseasonalsignal    half_seasonal_signal_value\nestimateoffsets       estimate_offsets_value\nestimatepostseismic   no\nestimateslowslipevent no\nRandomiseFirstGuess   no\nTimeNoiseStart        1\nDegreePolynomial      degree_polynomial_value\nLikelihoodMethod      FullCov\nJSON                  yes\n"
  
  # replace data_file_value by data_file
  tpl_1 = stringi::stri_replace( str = estimate_trend_tmpl, regex =  "data_file_value" , replacement = data_file)
  
  tpl_2 = stringi::stri_replace( str = tpl_1,regex =  "data_directory_value" , replacement = working_folder)
  
  tpl_3 = stringi::stri_replace( str = tpl_2,regex =  "output_file_value" , replacement = output_file)
  
  tpl_4 = stringi::stri_replace( str = tpl_3,regex =  "noise_models_value" , replacement = noise_model)
  
  tpl_5 = stringi::stri_replace( str = tpl_4,regex =  "seasonal_signal_value" , replacement = if (n_seasonal > 0) "yes" else "no")
  
  tpl_6 = stringi::stri_replace( str = tpl_5,regex =  "half_seasonal_signal_value" , replacement = if (n_seasonal > 1) "yes" else "no")
  
  tpl_7 = stringi::stri_replace( str = tpl_6,regex =  "estimate_offsets_value" , replacement = if (!is.null(x$jumps[1])) "yes" else "no")
  
  tpl_8 = stringi::stri_replace( str = tpl_7,regex =  "degree_polynomial_value" , replacement = 1)
  
  tpl_8
}






#' Estimate a stochastic model based on the MLE and the Hector implementation.
#' 
#' @param x A \code{gnssts} object
#' @param n_seasonal An \code{integer} specifying the number of seasonal component in the time series.
#' @param model_string A \code{string} specifying the model to be estimated.
#' @param cleanup  An \code{boolean} specifying if the files created by the estimation procedure should be cleaned.
#' @return A \code{gnsstsmodel} object.
#' @export
#' @importFrom rjson fromJSON
#' @importFrom stringi stri_rand_strings
#' @importFrom wv wvar
#' @examples 
#' \dontrun{
#' cola = PBO_get_station(station_name = "COLA", column = "dE", time_range = c(51130, 52000))
#' fit_mle = estimate_hector(x = cola,
#'                           n_seasonal = 1, 
#'                           model_string = "wn+matern")
#' 
#' }
#' 
estimate_hector <- function(
    x,
    n_seasonal = 1, 
    model_string,
    cleanup = TRUE
) {
  

  
  
  
  if (!("gnssts" %in% class(x))) {
    stop("x must be an object of type 'gnssts")
  }
  
  if (n_seasonal > 2) {
    stop("Hector supports only seasonal and halfseasonal signals ( 0 <= n_seasonal <= 1 )")
  }
  
  model = create_model_descriptor(model_string)
  
  # create temporary folders
  working_folder = paste(tempdir(), stri_rand_strings(n=1,length = 16), sep = "/")
  
  if (cleanup == F) {
    message(paste("Working in", working_folder))
  }
  
  #  create working folder
  dir.create(working_folder, showWarnings = F, recursive = T)
  
  # store time series
  write.gnssts(x, filename = paste(working_folder, "ts.mom", sep = "/"), format = "mom")
  
  # create hector configuration file
  cfg_file = paste(working_folder, "estimate.ctl", sep="/")
  output_file = paste(working_folder, "output.mom", sep="/")
  
  cfg = infuse_estimate_trend_tmp(data_file = "ts.mom",
                                  working_folder = working_folder,
                                  output_file = output_file,
                                  noise_model = gen_hector_model(model), 
                                  n_seasonal = n_seasonal, 
                                  x = x)
  
  # infuse hector configuration file using file saved in R/sysdata.rda
  # cfg = infuse(
  #   file_or_string  = estmatetrend_tmpl,
  #   list(
  #     DataFile = "ts.mom", 
  #     DataDirectory = working_folder, 
  #     OutputFile = output_file,
  #     NoiseModels = gen_hector_model(model),
  #     seasonalsignal = if (n_seasonal > 0) "yes" else "no",
  #     halfseasonalsignal = if (n_seasonal > 1) "yes" else "no",
  #     estimateoffsets = if (!is.null(x$jumps[1])) "yes" else "no",
  #     DegreePolynomial = 1
  #   )
  # )
  
  # write cfg file
  write(cfg, file = cfg_file)
  
  # run hector assuming that estimatetrend is accesible in the PATH
  cmd = sprintf("cd %s; %s '%s'", working_folder, "estimatetrend", cfg_file)
  
  timing = system.time({out = system(cmd, intern = TRUE)})
  
  json = fromJSON(file = paste(working_folder, "estimatetrend.json", sep="/"))
  
  # collect deterministic parameters
  
  # hector does not put the bias in the json file, parse the output
  bias_raw = find_one_output(out, "bias\\s+:\\s+([+-]?\\d+\\.\\d+) \\+/- ([+-]?\\d+\\.\\d+)")
  bias = as.numeric(bias_raw[1,2:3])
  
  beta_hat = c(
    bias[1],
    json[["trend"]] / 365.25, # hector gives the trend in unit/year
    json[["Sa_sin"]],
    json[["Sa_cos"]],
    json[["Ssa_sin"]], # if not enabled they are just null
    json[["Ssa_cos"]],
    json[["jumps_sizes"]]
  )
  
  beta_std = c(
    bias[2],
    json[["trend_sigma"]] / 365.25, # hector gives the trend in unit/year
    json[["Sa_sin_sigma"]],
    json[["Sa_cos_sigma"]],
    json[["Ssa_sin_sigma"]], # if not enabled they are just null
    json[["Ssa_cos_sigma"]],
    json[["jumps_sigmas"]]
  )
  
  names(beta_hat) <- c("bias", "trend", rep(c("A*cos(U)", "A*sin(U)"), n_seasonal), rep("jump", length(x$jumps)))
  
  # transform beta hat to vector
  beta_hat = as.vector(unlist(beta_hat))
  
  # collect stochastic parameters
  theta_hat = gen_pick_params_hector(model, json)
  
  if (cleanup == TRUE) {
    unlink(working_folder, recursive = TRUE)
  }
  
  # compute functional model, residuals and wv
  
  t_nogap = x$t[1]:tail(x$t,1)
  which_data = is.element(t_nogap, x$t)
  
  X = create_A_matrix(t_nogap, x$jumps, n_seasonal)
  
  x_hat = X %*% beta_hat
  
  rsd = x$y - X[which_data, ] %*% beta_hat # compute residuals where I have data
  
  wv_rsd = wvar(rsd)
  
  # create the output object
  
  ret = list()
  ret$model = model
  
  # functional parameters
  ret$beta_hat = beta_hat
  ret$beta_std = beta_std
  
  # stochastic parameter
  ret$theta_hat = theta_hat
  ret$theta_std = rep(NA, length(theta_hat)) # TODO: CI for theta not collected
  
  # original time series
  ret$input = list()
  ret$input$jumps = x$jumps
  ret$input$t_orig = x$t
  ret$input$x_orig = x$y
  
  # fitted functional model
  ret$output = list()
  ret$output$t_nogap = t_nogap
  ret$output$x_hat = x_hat
  
  residuals
  ret$residuals = rsd
  
  # ww of the residuals
  ret$wv_residuals = wv_rsd
  
  # timing
  ret$estimation_time = timing
  
  class(ret) <- "gnsstsmodel"
  
  ret
}

#' Remove outliers from a gnssts object using Hector 
#' 
#' @param x A \code{gnssts} object
#' @param n_seasonal An \code{integer} specifying the number of seasonal component in the time series.
#' @param IQ_factor the \code{IQ_factor} parameter in hector removeoutliers.ctl
#' @param cleanup  An \code{boolean} specifying if temporary files should be cleaned.
#' @return A \code{gnssts} object.
#' @export
#' @importFrom stringi stri_rand_strings
#' 
#' 
#' @examples 
#' phase =     0.45
#' amplitude = 2.5
#' sigma2_wn =       15
#' bias =            0
#' trend =           5/365.25
#' cosU =            amplitude*cos(phase)
#' sinU =            amplitude*sin(phase)
#' n= 2*365
#' # define time at which there are jumps
#' jump_vec =  c(100, 200)
#' jump_height = c(10, 20)
#' # generate residuals
#' eps = rnorm(n = n, sd = sqrt(sigma2_wn))
#' # add trend, gaps and sin
#' A = create_A_matrix(1:length(eps), jump_vec, n_seasonal =  1)
#' # define beta
#' x_0 = c(bias, trend, jump_height,  cosU,  sinU)
#' # create time series
#' yy = A %*% x_0 + eps
#' plot(yy, type="l")
#' n_outliers = 30
#' set.seed(123)
#' id_outliers=sample(150:350, size = n_outliers)
#' val_outliers = rnorm(n = n_outliers, mean = max(yy)+10, sd = 5)
#' yy[id_outliers] = val_outliers
#' plot(yy, type="l")
#' # save signal in temp
#' gnssts_obj = create.gnssts(t = 1:length(yy), y = yy, jumps = jump_vec)
#' \dontrun{
#' clean_yy = remove_outliers_hector(x=gnssts_obj, n_seasonal = 1)
#' plot(clean_yy$t, clean_yy$y, type="l")
#' }
remove_outliers_hector <- function(x, n_seasonal, IQ_factor = 3, cleanup = TRUE) {
  
  if (!("gnssts" %in% class(x))) {
    stop("x must be an object of type 'gnssts")
  }
  
  if (n_seasonal > 2) {
    stop("Hector supports only seasonal and halfseasonal signals ( 0 <= n_seasonal <= 1 )")
  }
  
  # create temporary folders
  working_folder = paste(tempdir(), stri_rand_strings(n=1,length = 16), sep = "/")
  
  if (cleanup == FALSE) {
    message(paste("Working in", working_folder))
  }
  
  dir.create(working_folder, showWarnings = FALSE, recursive = TRUE)
  
  # store time series
  
  write.gnssts(x, filename = paste(working_folder, "ts.mom", sep = "/"), format = "mom")

  cfg_file = paste(working_folder, "removeoutliers.ctl", sep="/")
  
  cfg = infuse_remove_outliers_tmp(data_file = "ts.mom",
                                   working_folder = working_folder,
                                   output_file = "ts_out.mom", 
                                   n_seasonal = n_seasonal, 
                                   IQ_factor = IQ_factor,
                                   x = x)
  
# infuse using the template removeoutliers_tmpl accesible in R/sysdata.rda
  # cfg = infuse(
  #   file_or_string = removeoutliers_tmpl,
  #   list(
  #     data_file = "ts.mom",
  #     data_dir = working_folder,
  #     output_file = "ts_out.mom",
  #     seasonalsignal = if (n_seasonal > 0) "yes" else "no",
  #     halfseasonalsignal = if (n_seasonal > 1) "yes" else "no",
  #     estimateoffsets = if (!is.null(x$jumps[1])) "yes" else "no",
  #     IQ_factor = sprintf('%.2f', IQ_factor)
  #   )
  # )
  
  write(cfg, file = cfg_file)
  
  # run hector assuming that removeoutliers is accesible in the PATH
  cmd = sprintf("cd %s; %s '%s'", working_folder, 'removeoutliers', cfg_file)
  
  timing = system.time({out = system(cmd, intern = TRUE)})
  
  y = read.gnssts(
    filename = paste(working_folder, "ts_out.mom", sep = "/"),
    format = "mom"
  )
  
  if (cleanup == TRUE) {
    unlink(working_folder, recursive = T)
  }
  
  y
}