library(car)
regPlots <- function(x, y, xlabs = "X", ylabs = "Y", mains = "Plot of X vs Y"){
  mod1 <- lm(y ~ x)
  plot(x, y, pch = 19, col = 'red', xlab = xlabs, ylab = ylabs, main = mains)
  abline(mod1$coef, lwd = 3, col = 'blue')
  mtext(paste(ylabs, "=", signif(mod1$coef[1], 3), " + ", signif(mod1$coef[2], 3), "*", ylabs), side = 3, line = 0.5)
  qqp(mod1$residuals, pch = 19, col = 'red', main = "Normal Quantile Plots of Residuals", ylab = ylabs)
  plot(mod1$residuals ~ fitted(mod1), pch = 19, col = 'red', xlab = "Fitted Values", ylab = "Residuals", main = "Fits vs. Residuals")
  abline(h = 0, col = 'blue', lwd = 3)
}