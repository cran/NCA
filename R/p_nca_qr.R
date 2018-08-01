p_nca_qr <-
function (loop.data, bn.data) {
  x <- loop.data$x
  y <- loop.data$y
  flip.y <- loop.data$flip.y
  # Make sure 0 <= qr.tau <= 1
  qr.tau <- loop.data$qr.tau
  if (qr.tau < 0 || qr.tau > 1) {
    qr.tau <- 0.95
  }

  if (!flip.y) {
    # Upper corners
    qr <- rq(y~x, tau=qr.tau)
  } else if (flip.y) {
    # Lower corner
    qr <- rq(y~x, tau=1-qr.tau)
  }

  intercept   <- unname(coef(qr)["(Intercept)"])
  slope       <- unname(coef(qr)["x"])
  ceiling     <- p_ceiling(loop.data, slope, intercept)
  effect      <- ceiling / loop.data$scope.area
  above       <- p_above(loop.data, slope, intercept)
  accuracy    <- p_accuracy(loop.data, above)
  fit         <- get_fit(ceiling, loop.data$ce_fdh_ceiling)
  ineffs      <- p_ineffs(loop.data, slope, intercept)
  bottleneck  <- p_bottleneck(loop.data, bn.data, slope, intercept)

  return(list(line=qr,
              slope=slope, intercept=intercept,
              ceiling=ceiling, effect=effect,
              above=above, accuracy=accuracy, fit=fit,
              ineffs=ineffs, bottleneck=bottleneck))
}