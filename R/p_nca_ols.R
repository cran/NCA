p_nca_ols <-
function (loop.data, ...) {
  x         <- loop.data$x
  y         <- loop.data$y
  ols       <- lm(y~x)
  intercept <- unname(coef(ols)["(Intercept)"])
  slope     <- unname(coef(ols)["x"])
  ceiling   <- p_ceiling(loop.data, slope, intercept)
  effect    <- ceiling / loop.data$scope.area
  above     <- p_above(loop.data, slope, intercept)
  accuracy  <- p_accuracy(loop.data, above)
  fit       <- 100 * ceiling / loop.data$ce_fdh_ceiling
  ineffs    <- p_ineffs(loop.data, slope, intercept)

  return(list(line=ols,
              slope=slope, intercept=intercept,
              ceiling=ceiling, effect=effect,
              above=above, accuracy=accuracy, fit=fit,
              ineffs=ineffs, bottleneck=NULL))
}