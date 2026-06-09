library(car)
regPlots <- function(x, y, xlabs = "X", ylabs = "Y", mains = "Plot of X vs Y", stdres = F){
  regdata <- na.omit(data.frame(x, y))
  mod1 <- lm(y ~ x, data = regdata)
  plot(y ~ x, data = regdata, pch = 19, col = 'red', xlab = xlabs, ylab = ylabs, main = mains)
  abline(mod1$coef, lwd = 3, col = 'blue')
  mtext(paste(ylabs, "=", signif(mod1$coef[1], 3), " + ", signif(mod1$coef[2], 3), "*", xlabs), side = 3, line = 0.5)
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