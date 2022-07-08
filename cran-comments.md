# Local check on Ubuntu 20.04

── R CMD check results ───────────────────── gmwmx 0.1.0 ────
Duration: 1m 15.4s

❯ checking dependencies in R code ... NOTE
  Namespace in Imports field not imported from: ‘simts’
    All declared Imports should be used.

❯ checking R code for possible problems ... NOTE
  estimate_gmwmx: warning in fit_base(theta_0 = theta_0, wv_emph =
    wv_rsd, model = model_string, method = method, maxit = maxit):
    partial argument match of 'model' to 'model_string'
  plot_stochastic: warning in legend(legend_position, legend =
    c("Empirical WV", "Estimated Theoretical WV", rownames(wvs)), col =
    c("black", cols), lt = c(1, 1, rep(1, nrow(wvs))), pch = c(1, NA,
    rep(NA, nrow(wvs))), horiz = F, cex = 0.75, bty = "n", bg =
    "transparent"): partial argument match of 'lt' to 'lty'

0 errors ✔ | 0 warnings ✔ | 2 notes ✖


# Rhub check
## macos-highsierra-release-cran

  Build ID:   gmwmx_0.1.0.tar.gz-6e379507631f48ef86dbdd21b1bec12a
  Platform:   macOS 10.13.6 High Sierra, R-release, CRAN's setup
  Submitted:  10m 49.6s ago
  Build time: 8m 44.3s

❯ checking dependencies in R code ... NOTE
  Namespace in Imports field not imported from: ‘simts’
    All declared Imports should be used.

❯ checking R code for possible problems ... NOTE
  estimate_gmwmx: warning in fit_base(theta_0 = theta_0, wv_emph =
    wv_rsd, model = model_string, method = method, maxit = maxit):
    partial argument match of 'model' to 'model_string'
  plot_stochastic: warning in legend(legend_position, legend =
    c("Empirical WV", "Estimated Theoretical WV", rownames(wvs)), col =
    c("black", cols), lt = c(1, 1, rep(1, nrow(wvs))), pch = c(1, NA,
    rep(NA, nrow(wvs))), horiz = F, cex = 0.75, bty = "n", bg =
    "transparent"): partial argument match of 'lt' to 'lty'

0 errors ✔ | 0 warnings ✔ | 2 notes ✖

## R-CMD-check on GitHub actions 

All jobs pass on 

- macOS-latest (release)
- ubuntu-latest (devel)
- ubuntu-latest (oldrel-1)
- ubuntu-latest (release)
- windows-latest (release)

see: https://github.com/SMAC-Group/gmwmx/actions/runs/2630550398


# Downstream dependencies
There are currently no downstream dependencies for this package.

