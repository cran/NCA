p_nca_cols <-
function (loop.data, bn.data) {
  x <- loop.data$x
  y <- loop.data$y
  flip.x <- loop.data$flip.x
  flip.y <- loop.data$flip.y

  if (!flip.x && !flip.y) {
    # Upper left corner
    cols <- lm(y + max(residuals(lm(y~x)) ) ~ x)
  } else if (flip.x && !flip.y) {
    # Upper right corner
    cols <- lm(y + max(residuals(lm(y~x)) ) ~ x)
  } else if (flip.x && flip.y) {
    # Lower right corner
    cols <- lm(y + min(residuals(lm(y~x)) ) ~ x)
  } else if (!flip.x && flip.y) {
    # Lower left corner
    cols <- lm(y + min(residuals(lm(y~x)) ) ~ x)
  }

  intercept   <- unname(coef(cols)["(Intercept)"])
  slope       <- unname(coef(cols)["x"])
  ceiling     <- p_ceiling(loop.data, slope, intercept)
  effect      <- ceiling / loop.data$scope.area
  above       <- p_above(loop.data, slope, intercept)
  accuracy    <- p_accuracy(loop.data, above)
  fit         <- get_fit(ceiling, loop.data$ce_fdh_ceiling)
  ineffs      <- p_ineffs(loop.data, slope, intercept)
  bottleneck  <- p_bottleneck(loop.data, bn.data, slope, intercept)

  return(list(line=cols,
              slope=slope, intercept=intercept,
              ceiling=ceiling, effect=effect,
              above=above, accuracy=accuracy, fit=fit,
              ineffs=ineffs, bottleneck=bottleneck))
}