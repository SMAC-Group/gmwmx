# Load pkg
library(simts)
library(gmwmx)
library(progress)
# we consider example the model considered in model 3
phase <- 0.45
amplitude <- 2.5
sigma2_wn <- 10
gamma2_rw = .5
bias <- 0
trend <- 5 / 365.25
cosU <- amplitude * cos(phase)
sinU <- amplitude * sin(phase)

# consider n years of daily observations
year <- 2
n <- year * 365


# define model for generating gaussian white noise + PLP
model_gaussian_wn_rw <- WN(sigma2 = sigma2_wn) + RW(gamma2 = gamma2_rw)

# generate data
# define time at which there are jumps
jump_vec <- c(200, 300, 500)
jump_height <- c(10, 15, 20)

# define seed
myseed <- 123

# add trend, gaps and sin
nbr_sin <- 1 # define number of sinusoidal process

# define A matrix
A <- create_A_matrix(1:n, jump_vec, n_seasonal = nbr_sin)

# define beta
x_0 <- c(bias, trend, cosU, sinU, jump_height)

# define number of Monte Carlo simulation
n_simu <- 100

mat_res = matrix(ncol=18, nrow=n_simu)

pb <- progress_bar$new(total = n_simu)

for(b in seq(n_simu)){
  
  # b = 4
  # fix seed for reproducibility
  set.seed(myseed + b)
  
  # generate residuals from a Gaussian White noise + Power law process
  eps <- simts::gen_gts(model = model_gaussian_wn_rw, n = n)
  # plot(wv::wvar(eps))
  
  # create time series
  yy <- A %*% x_0 + eps
  
  # create gnssts
  gnssts_obj <- create.gnssts(t = 1:length(yy), y = yy, jumps = jump_vec)
  
  # gmwmx 1 step
  fit_gmwmx <- estimate_gmwmx(
    x = gnssts_obj,
    model = "wn+rw",
    theta_0 = c(1, 1),
    n_seasonal = 1,maxit = 5000,
    k_iter = 1
  )
  

  # MLE
  fit_hector <- estimate_hector(
    x = gnssts_obj,
    model = "wn+rw",
    n_seasonal = 1
  )
  
  # save parameters
  mat_res[b, ] = c(  fit_gmwmx$beta_hat,
                     fit_gmwmx$theta_hat,
                     fit_hector$beta_hat,
                     fit_hector$theta_hat)

  pb$tick()
  
}


# boxplot
boxplot(mat_res[, c(8, 17)])
abline(h = sigma2_wn)
boxplot(mat_res[, c(9, 18)])
abline(h =gamma2_rw)

