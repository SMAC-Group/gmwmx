#' Print method for a \code{gnsstsmodel} object.
#'
#' @param x A \code{gnsstsmodel} object.
#' @param ... Additional graphical parameters.
#' @return No return value. Print a \code{gnsstsmodel} object.
#' @export
#'
print.gnsstsmodel <- function(x, ...) {
  cat("GNSS time series model\n\n")
  
  cat(paste(" * Model:", paste(x$x$names, collapse = " + "), "\n\n"))
  
  cat(" * Functional parameters:\n")
  for (i in seq_along(x$beta_hat)) {
    cat(sprintf("     %-30s : %+15.8f", names(x$beta_hat[i]), x$beta_hat[i]))
    if (!is.na(x$beta_std[i])) {
      cat(sprintf(" +/- %3.3f", x$beta_std[i]))
    }
    cat("\n")
  }
  
  cat("\n * Stochastc parameters:\n")
  for (i in seq_along(x$theta_hat)) {
    cat(sprintf("     %-30s : %+15.8f\n", names(x$theta_hat[i]), x$theta_hat[i]))
  }
  
  cat(sprintf("\n * Estimation time: %.2f s\n", x$estimation_time["elapsed"]))
  
}