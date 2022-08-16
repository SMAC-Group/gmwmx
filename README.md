<!-- badges: start -->
![example workflow](https://github.com/SMAC-Group/gmwmx/actions/workflows/R-CMD-check.yaml/badge.svg)
[![CRAN status](https://www.r-pkg.org/badges/version/gmwmx)](https://CRAN.R-project.org/package=gmwmx)
![](https://img.shields.io/github/last-commit/SMAC-Group/gmwmx) 
[<img src="https://s-a.github.io/license/img/agpl-3.0.svg" />](https://s-a.github.io/license/?license=agpl-3.0&fullname=Stephan%20Ahlf&year=2015&profile=https://github.com/s-a&projectUrl=https://github.com/s-a/license&projectName=License%20Demo "")
[![minimal R version](https://img.shields.io/badge/R%3E%3D-4.0.0-6666ff.svg)](https://cran.r-project.org/)
[![CRAN RStudio mirror
downloads](http://cranlogs.r-pkg.org/badges/last-day/gmwmx)](http://www.r-pkg.org/pkg/gmwmx)
[![CRAN RStudio mirror
downloads](http://cranlogs.r-pkg.org/badges/gmwmx)](http://www.r-pkg.org/pkg/gmwmx)
[![CRAN RStudio mirror
downloads](https://cranlogs.r-pkg.org/badges/grand-total/gmwmx)](http://www.r-pkg.org/pkg/gmwmx)

<!-- badges: end -->


# `gmwmx` Overview <a href="https://smac-group.com/"><img src="man/figures/logo.png" align="right" style="width: 20%; height: 20%"/></a>


The `gmwmx` `R` package implement the Generalized Method of Wavelet Moments with Exogenous Inputs estimator (GMWMX) introduced in [Cucci, D. A., Voirol, L., Kermarrec, G., Montillet, J. P., and Guerrier, S. (2022) <arXiv:2206.09668>](https://arxiv.org/abs/2206.09668) and provides functions to estimate times series models that can be expressed as linear models with correlated residuals. Moreover, the `gmwmx` package provides tools to compare and analyze estimated models and methods to easily compare results with the Maximum Likelihood Estimator (MLE) implemented in [Hector](https://teromovigo.com/hector/), allowing to replicate the examples and simulations considered in [Cucci, D. A., Voirol, L., Kermarrec, G., Montillet, J. P., and Guerrier, S. (2022) <arXiv:2206.09668>](https://arxiv.org/abs/2206.09668). In particular, this package implements a statistical inference framework for the functional and stochastic parameters of models such as those used to model Global Navigation Satellite System (GNSS) observations, enabling the comparison of the proposed method to the standard MLE estimates implemented in [Hector](https://teromovigo.com/hector/). 

Find the package vignettes and user's manual at the [package website](https://smac-group.github.io/gmwmx/index.html).

Below are instructions on how to install and make use of the `gmwmx`
package.

## Installation Instructions

The `gmwmx` package is available on both CRAN and GitHub. The CRAN
version is considered stable while the GitHub version is subject to
modifications/updates which may lead to installation problems or broken
functions. You can install the stable version of the `gmwmx` package
with:

``` r
install.packages("gmwmx")
```

For users who are interested in having the latest developments, the
GitHub version is ideal although more dependencies are required to run a
stable version of the package. Most importantly, users **must** have a
(`C++`) compiler installed on their machine that is compatible with R
(e.g. `Clang`).


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
In order to runs successfully functions that execute `Hector`, we assume that `Hector` is installed and available in the `PATH` of the installation where these functions are called. More precisely, when running either `estimate_hector()`, `remove_outliers_hector()`, `PBO_get_station()` or `PBO_get_offsets()`, we assume that `Hector`'s binaries executable `estimatetrend`, `removeoutliers` and `date2mjd` are located in a folder available in the `PATH` by `R`.

In order to make sure that these functions are available in the `PATH`, you can run `Sys.getenv("PATH")` and ensure that the directory that contains the executable binaries of `Hector` is listed in the `PATH`.

For Linux users that are on distributions supported by `Hector`, this can be easily done by:

1) Downloading `Hector`'s binaries for the corresponding OS [here](https://teromovigo.com/hector/).
2) Extracting the downloaded executable binaries and saving them in a folder, say `$HOME/app/hector/bin`.
3) Adding this folder to the system-wide `PATH` environment variable by modifying `/etc/environment`.
4) Ensuring that the corresponding folder is accessible by `R` with  `Sys.getenv("PATH")` after running the script and reassigning the new `PATH` to the `PATH` environment variable with `. /etc/environment` or equivalently with `source /etc/environment`.

```
> Sys.getenv("PATH")
[1] "$HOME/app/hector/bin:..."

```

### External `R` libraries

The `gmwmx` package relies on a limited number of external libraries, but notably on `Rcpp` and `RcppArmadillo` which require a `C++` compiler for installation, such as for example `gcc`.

## License

This source code is released under is the GNU AFFERO GENERAL PUBLIC LICENSE (AGPL) v3.0. 


## References
Cucci, D.A., Voirol, L., Kermarrec, G., Montillet, J.P. and Guerrier, S., 2022. The Generalized Method of Wavelet Moments with Exogenous Inputs: a Fast Approach for the Analysis of GNSS Position Time Series. arXiv preprint arXiv:2206.09668.

Guerrier, S., Skaloud, J., Stebler, Y. and Victoria-Feser, M.P., 2013. Wavelet-variance-based estimation for composite stochastic processes. Journal of the American Statistical Association, 108(503), pp.1021-1030.

