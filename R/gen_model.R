get_model_function = function(model, what) {
  tryCatch({
    f = get(paste(model, what, sep = "_"))
  }, error = function(e) { stop(sprintf("Model %s must define %s_%s function", model, model, what)) } )
  
  f
}


create_model_descriptor = function(name) {
  # get models
  base_models = strsplit(x = name, split = "\\+")[[1]]
  
  model = list(
    n_params = 0,
    names = c(),
    theta_subs = list(),
    v_tran = list(),
    v_tran_inv = list(),
    theo_wv = list(),
    autocovariance = list(),
    theta_to_exp = list()
  )
  
  i_th = 1
  
  for (i in seq_along(base_models)) {
    n_params_f = get_model_function(base_models[i], "n_params")
    
    model$names[i] = base_models[i]
    
    model$theta_subs[[i]] = i_th:(i_th+n_params_f()-1)
    i_th = i_th + n_params_f()
    
    model$v_tran[[i]] = get_model_function(base_models[i], "v_tran")
    model$v_tran_inv[[i]] = get_model_function(base_models[i], "v_tran_inv")
    model$theo_wv[[i]] = get_model_function(base_models[i], "theo_wv")
    
    model$autocovariance[[i]] = get_model_function(base_models[i], "autocovariance")
    
    model$theta_to_exp[[i]] = get_model_function(base_models[i], "theta_to_exp")
  }
  
  model$n_params = i_th - 1
  
  model
}

gen_v_tran = function(theta, model) {
  theta_t = rep(0, length(theta))
  
  for (i in seq_along(model$names)) {
    theta_t[model$theta_subs[[i]]] = model$v_tran[[i]](theta[model$theta_subs[[i]]])
  }
  
  theta_t
}

gen_v_tran_inv = function(theta, model) {
  theta_t = rep(0, length(theta))
  
  for (i in seq_along(model$names)) {
    theta_t[model$theta_subs[[i]]] = model$v_tran_inv[[i]](theta[model$theta_subs[[i]]])
  }
  
  theta_t
}

gen_theo_wv = function(theta, scales, model) {
  theo_wv = rep(0, length(scales))
  
  for (i in seq_along(model$names)) {
    theo_wv = theo_wv + model$theo_wv[[i]](theta[model$theta_subs[[i]]], scales)
  }
  
  theo_wv
}

gen_theo_wv_decomp = function(theta, scales, model) {
  theo_wvs = matrix(NA, nrow = length(model$names), ncol = length(scales))
  
  for (i in seq_along(model$names)) {
    theo_wvs[i,] = model$theo_wv[[i]](theta[model$theta_subs[[i]]], scales)
  }
  
  rownames(theo_wvs) <- model$names
  
  theo_wvs
}

gen_theta_to_exp = function(theta, model) {
  expr = model$theta_to_exp[[1]](theta[model$theta_subs[[1]]])
  
  if ( length(model$names) > 1) {
    for (i in 2:length(model$names)) {
      expr = as.expression(bquote(.(expr) + .(model$theta_to_exp[[i]](theta[model$theta_subs[[i]]]))))
    }
  }
  
  expr
}

# acf_to_sigma = function(acf) {
#   n = length(acf)
#   Rzz = c(rev(acf), acf[-1])
#   sigma = sapply(1:n, function(i){Rzz[(n-i+1):(2*(n-1)+1-i+1)]})
# }


gen_covariance = function(theta, n, model) {
  sigma = acf_to_sigma( model$autocovariance[[1]](theta[model$theta_subs[[1]]], n) )
  
  if ( length(model$names) > 1) {
    for (i in 2:length(model$names)) {
      sigma = sigma + acf_to_sigma( model$autocovariance[[i]](theta[model$theta_subs[[i]]], n) )
    }
  }
  
  sigma
}

gen_param_names = function(model) {
  pn = c() 
  
  for (i in 1:length(model$names)) {
    th_p = get_model_function(model$names[i], "param_names")()
    pn = c(pn, sprintf("%s_%s", model$names[i], th_p))
  }
  
  pn
}


gen_pick_params_hector = function(model, json) {
  params = c()
  
  for (i in 1:length(model$names)) {
    th_p = get_model_function(model$names[i], "pick_params_hector")(json)
    params = c(params, th_p)
  }
  
  names(params) <- gen_param_names(model)
  
  params
}


gen_hector_model = function(model) {
  models = c()
  
  for (i in 1:length(model$names)) {
    th_name = get_model_function(model$names[i], "name_hector")()
    models = c(models, th_name)
  }
  
  paste(models, collapse = " ")
}

