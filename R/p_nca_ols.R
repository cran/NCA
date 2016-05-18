p_nca_ols <-
function (loop.data, ...) {
  x         <- loop.data$x
  y         <- loop.data$y
  ols       <- lm(y~x)
  intercept <- unname(coef(ols)["(Intercept)"])
  slope     <- unname(coef(ols)["x"])
  ceiling   <- p_ceiling(loop.data, slope, intercept)
  effect    <- ceiling / loop.data$scope.area
  ineffs    <- p_ineffs(loop.data, slope, intercept)
  above     <- p_above(loop.data, slope, intercept)
  
  return(list(line=ols,
              slope=slope, intercept=intercept,
              ceiling=ceiling, effect=effect,
              ineffs=ineffs, above=above,
              bottleneck=NULL))
}