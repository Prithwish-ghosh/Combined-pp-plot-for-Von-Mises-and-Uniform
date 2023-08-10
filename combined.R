pp.unif.vm.plot = function (x, ref.line = TRUE, frac = NULL, xlab = "Von Mises and Unifrom Distribution", 
          ylab = "Empirical Dist VonMises and Uniform", col = NULL, col.inf = NULL, col1 = NULL,
          col2 = NULL,
          col.sup = NULL, ...) 
{
  x <- na.omit(x)
  if (length(x) == 0) {
    warning("No observations (at least after removing missing values)")
    return(NULL)
  }
  y <- sort(x%%(2 * pi))/(2 * pi)
  y1 = sort(x%%(2 * pi))
  n <- length(y)
  z <- (1:n)/(n + 1)
  mu <- circ.mean(y1)%%(2 * pi)
  kappa <- est.kappa(y1)
  if (is.null(col)) 
    col <- rep(1, n)
  else col <- rep(col, length.out = n)
  if (!is.null(frac)) {
    if (!is.numeric(frac) || (frac < 0 | frac > 1)) {
      stop("'frac' must be in the interval [0,1]")
    }
    f <- round(frac * n)
    if (f) {
      zm <- -1 + ((n - f + 1):n)/(n + 1)
      zp <- 1 + (1:f)/(n + 1)
      ym <- -1 + y[(n - f + 1):n]
      yp <- 1 + y[1:f]
      y <- c(ym, y, yp)
      z <- c(zm, z, zp)
      if (is.null(col.inf)) 
        col.inf <- rep(2, f)
      else col.inf <- rep(col.inf, length.out = f)
      if (is.null(col.sup)) 
        col.sup <- rep(2, f)
      else col.sup <- rep(col.sup, length.out = f)
      col <- c(col.inf, col, col.sup)
    }
  }
  a <- c(1:n)
  for (i in 1:n) {
    a[i] <- pvm(y1[i], mu, kappa)
  }
  plot.default(z, y, xlab = xlab, ylab = ylab, col = col, lwd = 0.3, lty = 1 ,...)
  lines(z , a , col = col1, lty = 4 , lwd = 4)
  legend("topleft", legend=c("the predicting line", "Uniform fit" , "VonMises Fit"),
         col=c(col2,col , col1), lty=c(1,4),lwd=c(2,2), cex=0.59,
         box.lty=0)
  if (ref.line) {
    abline(0, 1 , col = col2 , lwd = 3.0)
    if (!is.null(frac)) {
      abline(h = c(0, 1), lty = 3)
      abline(v = c(0, 1), lty = 3)
    }
  }
}

pp.unif.vm.plot(completed_data$fireball.Latitude, col = "red" , col1 = "green" , col2 = "blue")
