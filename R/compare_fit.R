#' Compare graphically two \code{gnsstsmodel} objects.
#' 
#' @param fit_1 A \code{gnsstsmodel} object.
#' @param fit_2 A \code{gnsstsmodel} object.
#' @param main A \code{string} specifying the plot title.
#' @param y_unit A \code{string} specifying the y axis label. 
#' @param x_unit A \code{string} specifying the x axis label.
#' @return No return value. Produce a plot comparing two estimated models.
#' @export
#' 
#' @examples 
#' \dontrun{
#' data(cola)
#' fit_gmwmx_1 = estimate_gmwmx(x = cola,
#'                              theta_0 = c(0.1,0.1,0.1,0.1), 
#'                              n_seasonal = 1, 
#'                              model_string = "wn+matern")
#' fit_gmwmx_2 = estimate_gmwmx(x = cola,
#'                              theta_0 = c(0.1,0.1,0.1), 
#'                              n_seasonal = 1, 
#'                              model_string = "wn+powerlaw")
#' compare_fits(fit_gmwmx_1, fit_gmwmx_2)
#' }
compare_fits <- function(fit_1, fit_2,   
                         main = NULL,
                         y_unit = "mm",
                         x_unit = "days"){
  
  # stop if not same original signal in both fits
  if(!all(fit_1$input$x_orig == fit_2$input$x_orig)){
    stop("Provided fits are not estimated on the same signals.")
  }
  
  # waning if not same model
  if(!all(fit_1$model$names == fit_2$model$names)){
    warning("Provided fits do not esimate the same model.")
  }
  
  # store original plot conf
  oldpar <- par(no.readonly = TRUE)
  on.exit(par(oldpar)) 
  # oldpar = oldpar[!(names(oldpar) %in% c("pin", "cin", "cra", "csi", "cxy", "din", "page"))]
  
  
  # define layout matrix
  layout_mat = matrix(c(1,1,2,3), byrow = TRUE, ncol=2, nrow=2)
  layout(mat = layout_mat,
         heights = c(1.5, 2), # Heights of the two rows
         widths = c(2, 2)) # Widths of the two columns
  
  # plot time series using fit_1 
  plot(
    fit_1$input$t_orig, 
    fit_1$input$x_orig, 
    type = "l", 
    main = if (is.null(main)) "GNSS ts model" else main, 
    ylab = paste("[", y_unit, "]", sep = ""), 
    xlab = paste("Time [", x_unit, "]", sep = ""),
    
  )
  
  
  # plot grey bars when there are time gaps
  dt = min(diff(fit_1$input$t_orig))
  t_nogap = seq(fit_1$input$t_orig[1], tail(fit_1$input$t_orig,1), dt)
  
  for (i in 1:length(t_nogap)) {
    if (any(abs(fit_1$input$t_orig - t_nogap[i])< 1e-6) == F) {
      rect(xleft = t_nogap[i]-dt/2, xright = t_nogap[i]+dt/2, ybottom = -2e3, ytop = 2e3, col = "grey", border = NA)
    }
  }
  
  # plot the estimated functional model 
  lines(fit_1$output$t_nogap, fit_1$output$x_hat, col = gg_color_hue(1), lw=2)
  
  # plot the estimated functional model for fit_2
  lines(fit_2$output$t_nogap, fit_2$output$x_hat, col = "#619CFF", lw=2)
  
  
  # add legend for 
  legend("bottomright",
         lty=1, lwd=1, 
         legend=c("fit 1", "fit 2"),
         # legend=c(deparse(quote(fit_1)),
         #          deparse(quote(fit_1))),
         col=c("#F8766D", "#619CFF"),
         bty="n")
  
  # vertical lines for the jumps  
  abline(v = fit_1$input$jumps, col = gg_color_hue(2)[2])
  
  # plot wv of residuals of fit 1
  plot_stochastic(
    wv_emph = fit_1$wv_residuals,
    theta_hat = fit_1$theta_hat,
    model = fit_1$model,
    plot_ig = F
  )
  
  # plot wv of residuals of fit 2
  plot_stochastic(
    wv_emph = fit_2$wv_residuals,
    theta_hat = fit_2$theta_hat,
    model = fit_2$model,
    plot_ig = F
  )
  

  
}

