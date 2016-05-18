p_nca_qr <-
function (loop.data, bn.data) {
  x <- loop.data$x
  y <- loop.data$y
  flip.x <- loop.data$flip.x
  flip.y <- loop.data$flip.y

  if (!flip.y) {
    # Upper corners
    qr <- rq(y~x, tau=0.95)
  } else if (flip.y) {
    # Lower corner
    qr <- rq(y~x, tau=0.05)
  }

  intercept   <- unname(coef(qr)["(Intercept)"])
  slope       <- unname(coef(qr)["x"])
  ceiling     <- p_ceiling(loop.data, slope, intercept)
  effect      <- ceiling / loop.data$scope.area
  ineffs      <- p_ineffs(loop.data, slope, intercept)
  above       <- p_above(loop.data, slope, intercept)
  bottleneck  <- p_bottleneck(loop.data, bn.data, slope, intercept)

  return(list(line=qr,
              slope=slope, intercept=intercept,
              ceiling=ceiling, effect=effect,
              ineffs=ineffs, above=above,
              bottleneck=bottleneck))
}