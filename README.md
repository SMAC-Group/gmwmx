
<!-- README.md is generated from README.Rmd. Please edit that file -->

![](https://img.shields.io/github/last-commit/SMAC-Group/gmwmx) 

[<img src="https://s-a.github.io/license/img/agpl-3.0.svg" />](https://s-a.github.io/license/?license=agpl-3.0&fullname=Stephan%20Ahlf&year=2015&profile=https://github.com/s-a&projectUrl=https://github.com/s-a/license&projectName=License%20Demo "")  

[![minimal R
version](https://img.shields.io/badge/R%3E%3D-4.0.0-6666ff.svg)](https://cran.r-project.org/)

# `gmwmx` Overview <a href="https://smac-group.com/"><img src="man/figures/logo.png" align="right" style="width: 20%; height: 20%"/></a>


The `gmwmx` `R` package implement the Generalized Method of Wavelet Moments with Exogenous Inputs estimator (GMWMX) introduced in [Cucci et al. (2022)](https://arxiv.org/abs/2206.09668) and provides functions to estimate times series models that can be expressed as linear models with correlated residuals. Moreover, the `gmwmx` package provides tools to compare and analyze estimated models and methods to easily compare results with the Maximum Likelihood Estimator (MLE) implemented in [Hector](https://teromovigo.com/hector/), allowing to replicate the examples and simulations considered in [Cucci et al. (2022)](https://arxiv.org/abs/2206.09668). In particular, this package implements a statistical inference framework for the functional and stochastic parameters of models such as those used to model Global Navigation Satellite System (GNSS) observations, enabling the comparison of the proposed method to the standard MLE estimates implemented in [Hector](https://teromovigo.com/hector/). 

Below are instructions on how to install and make use of the `gmwmx`
package.

## Installation Instructions

The `gmwmx` package is available only on GitHub at the moment. The latest
version can be installed with:

``` r
# Install dependencies
install.packages(c("devtools"))

# Install/Update the package from GitHub
devtools::install_github("SMAC-Group/gmwmx")

# Install the package with Vignettes/User Guides 
devtools::install_github("SMAC-Group/gmwmx", build_vignettes = TRUE)
```

## External dependencies

### `Hector`
In order to runs successfully functions that execute `Hector`, we assume that `Hector` is installed and available in the `PATH` of the installation where these functions are called. More precisely, when running either `estimate_hector()` or `remove_outliers_hector()`, we assume that `Hector`'s binaries executables `estimatetrend`, `date2mjd` and `removeoutliers` are compiled and located in a folder available in the `PATH`.

In order to make sure that these functions are available in the `PATH`, you can run `Sys.getenv("PATH")` and ensure that the directory that contains the executable binaries of `Hector` is listed in the `PATH`.

For Linux users that are on distributions supported by `Hector`, this can be easily done by:

1) Downloading `Hector`'s binaries for the corresponding OS [here](https://teromovigo.com/hector/).
2) Extracting the downloaded executable binaries and saving them in a folder, say `$HOME/app/hector/bin`.
3) Adding this folder to the system-wide `PATH` environment variable by modifying `/etc/environment`.
4) Ensuring that the corresponding folder is accessible by `R` with  `Sys.getenv("PATH")` after running the script and reassigning the new `PATH` to the `PATH` environment variable with `. /etc/environment` or equivalently with `source /etc/environment`.

### External `R` libraries

The `gmwmx` package relies on a limited number of external libraries, but notably on `Rcpp` and `RcppArmadillo` which require a `C++` compiler for installation and on the `R` package `infuser` which is not available on `CRAN` and has to be installed directly from its [GitHub repo](https://github.com/Bart6114/infuser) following the provided instructions.

## License

The license this source code is released under is the GNU AFFERO GENERAL PUBLIC LICENSE (AGPL) v3.0. See the LICENSE file for full text. 


## References
Cucci, D.A., Voirol, L., Kermarrec, G., Montillet, J.P. and Guerrier, S., 2022. The Generalized Method of Wavelet Moments with Exogenous Inputs: a Fast Approach for the Analysis of GNSS Position Time Series. arXiv preprint arXiv:2206.09668.

Guerrier, S., Skaloud, J., Stebler, Y. and Victoria-Feser, M.P., 2013. Wavelet-variance-based estimation for composite stochastic processes. Journal of the American Statistical Association, 108(503), pp.1021-1030.

