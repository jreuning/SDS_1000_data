library(car)
regPlots <- function(x, y, xlabs = "X", ylabs = "Y", mains = "Plot of X vs Y", stdres = F, resplots = T, bands = F, xlims = 0) {
  regdata <- na.omit(data.frame(x, y))
  mod1 <- lm(y ~ x, data = regdata)
  if (bands == T){
    if (xlims == 0){ newX <-  seq(min(regdata$x), max(regdata$y), length.out = 100) }
    else { newX <-  seq(xlims[1], xlims[2], length.out = 100)}
    #Make this into a one-variable dataframe.
    ndata <-  data.frame(x = newX)
    #Get confidence and prediction bands
    confbands1 <- predict(mod1, interval = "confidence", newdata = ndata, level = .95) #.95 is the default level
    predbands1 <- predict(mod1, interval = "prediction", newdata = ndata) 
  }
  
  plot(y ~ x, data = regdata, pch = 19, col = 'red', 
       xlab = xlabs, 
       ylab = ylabs, 
       main = mains)
  abline(mod1$coef, lwd = 3, col = 'blue')
  mtext(paste(ylabs, "=", signif(mod1$coef[1], 3), " + ", signif(mod1$coef[2], 3), "*", xlabs), side = 3, line = 0.5)
  if (bands == T){
    
    lines(newX, confbands1[, "lwr"], lty = 2, col = 'blue',lwd = 2)
    lines(newX, confbands1[, "upr"], lty = 2, col = 'blue',lwd = 2)
    
    lines(newX, predbands1[, "lwr"], lty = 3, col = 'red',lwd = 2)
    lines(newX, predbands1[, "upr"], lty = 3, col = 'red',lwd = 2)
    
    legend("topleft", c("Conf. Bands", "Pred. Bands"), lty = c(2,3), lwd = 3, col = c("blue", "red"))  
    
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



