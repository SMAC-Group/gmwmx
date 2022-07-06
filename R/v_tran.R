# ------------------- variable transformations


transfo_logit <- function(x){
  (1/(1 + exp(-x)))
}


inv_transfo_logit <- function(x){
  (log(x/(1-x)))
}


transfo_logit_pi_2 <- function(x){
  ((1/(1 + exp(-x)))) * pi/2
}


inv_transfo_logit_pi_2 <- function(x){
  -log(pi/(2*x) - 1)
}


transfo_exp <- function(x){
  exp(x)
}


inv_transfo_exp <- function(x){
  log(x)
}