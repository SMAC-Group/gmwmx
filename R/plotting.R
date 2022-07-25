#' @importFrom grDevices hcl
gg_color_hue <- function(n, alpha) {
  hues = seq(15, 375, length = n + 1)
  hcl(h = hues, l = 65, c = 100, alpha = alpha)[1:n]
}

plot_stochastic = function(wv_emph, theta_hat, model, theta_0, yl = NULL, plot_ig=T, ci = TRUE, legend_position ="topright") {

  if(is.null(yl)){
    yl = c(min(wv_emph$ci_low), max(wv_emph$ci_high ))
    
  }
  
  plot(NA,
       ylim = yl,
       xlim = c(min(wv_emph$scales), max(wv_emph$scales)),
       main = gen_theta_to_exp(theta_hat, model),
       ylab = "WV",
       xlab = "scales",
       log = "xy",
       xaxt = "n",
       yaxt = "n")
  
  # powers of 2
  #axis(1, at=wv_emph$scales, labels=log2(wv_emph$scales))
  
  # powers of 10
  p10 = floor(log10(wv_emph$scales[1])):ceiling(log10(tail(wv_emph$scales,1)))
  axis(1, at=10^p10, labels=parse(text=sprintf("10^%.0f",p10)))
  
  # add polygon ci
  if(ci ==T){
    polygon(x = c(wv_emph$scales, rev(wv_emph$scales)),
            y = c(wv_emph$ci_low, rev(wv_emph$ci_high)),
            col = "#E5EBF2",
            border = NA)
  }
  
  
  ylab = floor(log10(yl[1])):ceiling(log10(yl[2]))
  axis(2, at=10^ylab, labels=parse(text=sprintf("10^%.0f",ylab)))
  
  # add grid (at scales)
  # abline(v=wv_emph$scales, col="grey", lt=2)
  abline(v=10^p10, col="grey", lt=2)
  
  abline(h=10^ylab, col="grey", lt=2)

  lines(wv_emph$scales, wv_emph$variance, type="b", col = "black")

  wvs = gen_theo_wv_decomp(theta_hat, wv_emph$scales, model)
  cols = gg_color_hue(1+nrow(wvs), 1)
    
  for (i in 1:nrow(wvs)) {
    lines(wv_emph$scales, wvs[i,], type="l", col = cols[1+i], lw=1)
  }
  
  lines(
    wv_emph$scales, 
    gen_theo_wv(theta_hat, wv_emph$scales, model), 
    type="l", col = cols[1], lw=2
  )
  
  if (plot_ig) {
    wvs = gen_theo_wv_decomp(theta_0, wv_emph$scales, model)

    for (i in 1:nrow(wvs)) {
      lines(wv_emph$scales, wvs[i,], type="l", col = cols[1+i], lw=1, lt = 2)    
    }
    
    lines(
      wv_emph$scales, 
      gen_theo_wv(theta_0, wv_emph$scales, model), 
      type="l", col = cols[1], lw=2, lt = 2
    )
  }

  legend(
    legend_position,
    legend=c("Empirical WV", "Estimated Theoretical WV", rownames(wvs)),
    col = c("black", cols),
    lty  = c(1, 1, rep(1, nrow(wvs))),
    pch = c(1, NA, rep(NA, nrow(wvs))),
    horiz = F,
    cex = 0.75, bty="n", bg="transparent"
  )
}


#' Plotting method for a \code{gnsstsmodel} object.
#' 
#' @param x A \code{gnsstsmodel} object.
#' @param main A \code{string} specifying the title of the plot.
#' @param y_unit A \code{string} specifying the y axis label of the plot.
#' @param x_unit A \code{string} specifying the x axis label of the plot.
#' @param legend_position A \code{string} specifying the legend position of the plot.
#' @param legend_position_wv A \code{string} specifying the legend position for the wv plot.
#' @param ... Additional graphical parameters.
#' @return No return value. Plot a \code{gnsstsmodel} object.
#' @export
#' @importFrom grDevices hcl
#' @importFrom graphics abline axis layout legend lines mtext par points polygon rect text
#' 
#' @examples 
#' data(cola)
#' fit_gmwmx = estimate_gmwmx(x = cola,
#'                            theta_0 = c(0.1,0.1,0.1,0.1), 
#'                            n_seasonal = 1, 
#'                            model_string = "wn+matern")
#' plot(fit_gmwmx)
plot.gnsstsmodel <- function(
  x, 
  main = NULL, 
  y_unit = "mm",
  x_unit = "days",
  legend_position = "bottomright",
  legend_position_wv = "bottomleft",
  ...
) {


  # store original plot conf
  oldpar <- par(no.readonly = TRUE)
  on.exit(par(oldpar)) 
  # oldpar = oldpar[!(names(oldpar) %in% c("pin", "cin", "cra", "csi", "cxy", "din", "page"))]
  
  # define parameters plotting
  par(mfrow=c(2,1))
  
  # extract trend
  estimated_trend = x$beta_hat["trend"]
  
  plot(
    x$input$t_orig, 
    x$input$x_orig, 
    type = "l", 
    main = "", 
    ylab = paste("[", y_unit, "]", sep = ""), 
    xlab = paste("Time [", x_unit, "]", sep = ""),
    # ylim = plot_yl, 
    # xaxt = "n"
  )
  
  mtext(text=bquote("Trend: " ~ .(round(estimated_trend*365.25, 2)) ~" mm/year"), side=3, line = 0)

  # add title for plot
  mtext(text=if (is.null(main)) "GNSS ts model" else main, side=3, line = 2)
  # if (robust != F) {
  #   for (i in 1:length(t_orig)) {
  #     rect(xleft = t_orig[i]-0.5, xright = t_orig[i]+0.5, ybottom = -2e3, ytop = 2e3, col = gg_color_hue(2, 1-weights[i]), border = NA)
  #   }
  # }
  
  # plot grey bars when there are time gaps
  dt = min(diff(x$input$t_orig))
  t_nogap = seq(x$input$t_orig[1], tail(x$input$t_orig,1), dt)
  
  for (i in 1:length(t_nogap)) {
    if (any(abs(x$input$t_orig - t_nogap[i])< 1e-6) == F) {
      rect(xleft = t_nogap[i]-dt/2, xright = t_nogap[i]+dt/2, ybottom = -2e3, ytop = 2e3, col = "grey", border = NA)
    }
  }
  
  # plot the estimated functional model
  
  lines(x$output$t_nogap, x$output$x_hat, col = "#F8766D", lw=2)

  # x axis from julian day to years  
  # years = seq(1980,2020)
  # xat = sapply(years, function(y){ymdhms_to_j(y,0,0,0,0,0)})
  # axis(1, at = xat, labels = years)

  # vertical lines for the jumps  
  abline(v = x$input$jumps, col = "#00BFC4")
  

  
  # add the trend
  #  get number of seasonal
  # nbr_seasonal = sum(names(x$beta_hat) == "A*sin(U)")
  # identify estimated jumps
  id_jump = which(names(x$beta_hat)=="jump")
  X_mat = create_A_matrix(t_nogap = x$output$t_nogap, jumps = x$input$jumps, n_seasonal = 0)
  beta = c(x$beta_hat["bias"], x$beta_hat["trend"], x$beta_hat[id_jump])
  estimated_trend = X_mat %*% beta
  
  # add lines of estimated trend
  lines(x$output$t_nogap, estimated_trend, col = "#1fcc1f", lw=2)
  
  # add estimated jumps
  if (length(id_jump) > 0) {
    y_pos = max(x$input$x_orig)
    delta_x = length( x$input$t_orig)/ (length( x$input$t_orig)/100)
    for(i in seq(length(id_jump))){
      text(x = x$input$jumps[i]- delta_x, y = y_pos, labels = round(x$beta_hat[id_jump[i]], 2), cex = .7)
    }
  }

  # legend
  legend(legend_position, lwd=1, lty=1, 
         col=c("#00BFC4", "#F8766D", "#1fcc1f"),
         bty="n",
         cex=.6,
         legend = c("Specified jumps", "Estimated functional model", "Estimated trend"))
  
  
  
  
  plot_stochastic(
    wv_emph = x$wv_residuals,
    theta_hat = x$theta_hat,
    model = x$model,
    plot_ig = F, legend_position = legend_position_wv
    # theta_0 = theta_0
  )
  

}



ellipsem <- function (mu, amat, c2, npoints = 100, showcentre = TRUE, ...) {
  if (all(dim(amat) == c(2, 2))) {
    eamat <- eigen(amat)
    hlen <- sqrt(c2/eamat$val)
    theta <- angle(eamat$vec[1, 1], eamat$vec[2, 1])
    ellipse(hlen[1], hlen[2], theta, mu[1], mu[2], npoints = npoints,
            ...)
    if (showcentre)
      points(mu[1], mu[2], pch = 3)
  }
  invisible()
}


ellipse <- function (hlaxa = 1, hlaxb = 1, theta = 0, xc = 0, yc = 0, newplot = FALSE, npoints = 100, ...) {
  a <- seq(0, 2 * pi, length = npoints + 1)
  x <- hlaxa * cos(a)
  y <- hlaxb * sin(a)
  alpha <- angle(x, y)
  rad <- sqrt(x^2 + y^2)
  xp <- rad * cos(alpha + theta) + xc
  yp <- rad * sin(alpha + theta) + yc
  if (newplot)
    plot(xp, yp, type = "l", ...)
  else lines(xp, yp, ...)
  invisible()
}

angle <- function (x, y) {
  angle2 <- function(xy) {
    x <- xy[1]
    y <- xy[2]
    if (x > 0) {
      atan(y/x)
    }
    else {
      if (x < 0 & y != 0) {
        atan(y/x) + sign(y) * pi
      }
      else {
        if (x < 0 & y == 0) {
          pi
        }
        else {
          if (y != 0) {
            (sign(y) * pi)/2
          }
          else {
            NA
          }
        }
      }
    }
  }
  apply(cbind(x, y), 1, angle2)
}