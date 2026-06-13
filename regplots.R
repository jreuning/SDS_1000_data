library(car)
library(PerformanceAnalytics)
regPlots <- function(x, y, xlabs = "X", ylabs = "Y", mains = "Plot of X vs Y", fitplot = T, stdres = F, resplots = T, bands = F, xlims = c(NA, NA)) {
  regdata <- na.omit(data.frame(x, y))
  mod1 <- lm(y ~ x, data = regdata)
  ylims <-  range(regdata$y)
  if (bands == T){
    if (is.na(xlims[1])){ newX <-  seq(min(regdata$x), max(regdata$y), length.out = 100) }
    else { newX <-  seq(xlims[1], xlims[2], length.out = 100)}
    #Make this into a one-variable dataframe.
    ndata <-  data.frame(x = newX)
    #Get confidence and prediction bands
    confbands1 <- predict(mod1, interval = "confidence", newdata = ndata, level = .95) #.95 is the default level
    predbands1 <- predict(mod1, interval = "prediction", newdata = ndata) 
    ylims <- range(predbands1)
  }
  if (is.na(xlims[1])) {  xlims <- range(regdata$x)}  
  if (fitplot == T){
    plot(y ~ x, data = regdata, pch = 19, col = 'red', 
       xlab = xlabs, 
       ylab = ylabs, 
       main = mains,
       ylim = ylims,
       xlim = xlims)
   abline(mod1$coef, lwd = 3, col = 'green')
   mtext(paste(ylabs, "=", signif(mod1$coef[1], 3), " + ", signif(mod1$coef[2], 3), "*", xlabs), side = 3, line = 0.5)
   if (bands == T){
    lines(newX, confbands1[, "lwr"], lty = 2, col = 'red',lwd = 2)
    lines(newX, confbands1[, "upr"], lty = 2, col = 'red',lwd = 2)
    lines(newX, predbands1[, "lwr"], lty = 3, col = 'blue',lwd = 2)
    lines(newX, predbands1[, "upr"], lty = 3, col = 'blue',lwd = 2)
    legend("topleft", c("Conf. Bands", "Pred. Bands"), lty = c(2,3), lwd = 3, col = c("red", "blue"))  
   } 
  }
  if (resplots == T){
    qqp(mod1$residuals, pch = 19, col = 'red', main = "Normal Quantile Plots of Residuals", ylab = ylabs)
    if (stdres == F){
       plot(mod1$residuals ~ fitted(mod1), pch = 19, col = 'red', xlab = "Fitted Values", ylab = "Residuals", main = "Fits vs. Residuals")
       abline(h = 0, col = 'blue', lwd = 3)
    }
    else {
      plot(rstandard(mod1) ~ fitted(mod1), pch = 19, col = 'red', xlab = "Fitted Values", ylab = "Stand. Resid.", main = "Fits vs. Stand. Resid.")
      abline(h = 0, col = 'blue', lwd = 3) 
      abline(h = c(-2, 2), col = 'green', lwd = 2, lty = 2) 
      abline(h = c(-3, 3), col = 'red', lwd = 2, lty = 2) 
      text(fitted(mod1)[abs(rstandard(mod1)) > 2], rstandard(mod1)[abs(rstandard(mod1)) > 2], rownames(regdata)[abs(rstandard(mod1)) > 2])
    }
  }  
}

matrixPlot <- function (R, histogram = TRUE, method = c("pearson", "kendall", 
                                                        "spearman"), pch = 19, col = 'blue', ...) 
{
  x <- checkData(R, method = "matrix")
  if (missing(method)) 
    method <- method[1]
  cormeth <- method
  dotargs <- list(...)
  c_cex <- if ("cex.cor" %in% names(dotargs)) 
    dotargs$cex.cor
  else NULL
  panel.cor <- function(x, y, digits = 2, prefix = "", use = "pairwise.complete.obs", 
                        method = cormeth, ...) {
    usr <- par("usr")
    on.exit(par(usr = usr))
    par(usr = c(0, 1, 0, 1))
    r <- cor(x, y, use = use, method = method)
    txt <- format(c(r, 0.123456789), digits = digits)[1]
    txt <- paste(prefix, txt, sep = "")
    if (is.null(c_cex)) {
      cex <- 0.8/strwidth(txt)
    }
    else {
      cex <- c_cex
    }
    test <- cor.test(as.numeric(x), as.numeric(y), method = method)
    Signif <- symnum(test$p.value, corr = FALSE, na = FALSE, 
                     cutpoints = c(0, 0.001, 0.01, 0.05, 0.1, 1), symbols = c("***", 
                                                                              "**", "*", ".", " "))
    text(0.5, 0.5, txt, cex = cex * (abs(r) + 0.3)/1.3)
    text(0.8, 0.8, Signif, cex = cex, col = 2)
  }
  f <- function(t) {
    dnorm(t, mean = mean(x), sd = sd.xts(x))
  }
  panel.smooth.custom <- function(x, y, col = par("col"), bg = NA, 
                                  pch = par("pch"), cex = 1, col.smooth = "red", span = 2/3, 
                                  iter = 3, ...) {
    if (cormeth %in% c("spearman", "kendall")) {
      x_rk <- rank(x, na.last = "keep")
      y_rk <- rank(y, na.last = "keep")
      x <- min(x, na.rm = TRUE) + (x_rk - min(x_rk, na.rm = TRUE))/(max(x_rk, 
                                                                        na.rm = TRUE) - min(x_rk, na.rm = TRUE)) * (max(x, 
                                                                                                                        na.rm = TRUE) - min(x, na.rm = TRUE))
      y <- min(y, na.rm = TRUE) + (y_rk - min(y_rk, na.rm = TRUE))/(max(y_rk, 
                                                                        na.rm = TRUE) - min(y_rk, na.rm = TRUE)) * (max(y, 
                                                                                                                        na.rm = TRUE) - min(y, na.rm = TRUE))
    }
    points(x, y, pch = pch, col = col, bg = bg, cex = cex)
    ok <- is.finite(x) & is.finite(y)
    if (any(ok)) {
      lines(stats::lowess(x[ok], y[ok], f = span, iter = iter), 
            col = col.smooth, ...)
    }
  }
  dotargs <- list(...)
  dotargs$method <- NULL
  dotargs$cex.cor <- NULL
  hist.panel <- function(x, ... = NULL) {
    par(new = TRUE)
    hist(x, col = "light gray", probability = TRUE, axes = FALSE, 
         main = "", breaks = "FD")
    lines(density(x, na.rm = TRUE), col = "red", lwd = 1)
    rug(x)
  }
  title_str <- c("Matrix Plot")
  pairs_args <- c(list(x = x, gap = 0, upper.panel = panel.smooth.custom, 
                       lower.panel = panel.cor, pch = pch), dotargs)
  if (histogram) 
    pairs_args$diag.panel <- hist.panel
  if (!hasArg("main")) 
    pairs_args$main <- title_str
  do.call(pairs, pairs_args)
  p <- recordPlot()
  return(invisible(p))
}



resPlots <- function(model, label){
  
  #Normal quantile plot of studentized residuals
  qqp(rstandard(model), pch = 19, main = paste("NQ Plot of Standardized Residuals,", label))
  
  #plot of fitted vs. studentized residuals
  plot(rstandard(model) ~ model$fitted.values, 
       pch = 19, 
       col = 'red', 
       xlab = "Fitted Values", 
       ylab = "Studentized Residuals",
       main = paste("Fits vs. Studentized Residuals,", label))
  abline(h = 0, lwd = 3)
  abline(h = c(2,-2), lty = 2, lwd = 2, col="blue")
  abline(h = c(3,-3), lty = 2, lwd = 2, col="green")
  
}