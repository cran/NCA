nca_qr <-
function (loop.data, ...) {
  x         <- loop.data$x
  y         <- loop.data$y
  # TODO Make tau configurable?
  qr        <- rq(y~x, tau=0.95)
  
  intercept <- unname(coef(qr)["(Intercept)"])
  slope     <- unname(coef(qr)["x"])
  ceiling   <- p_ceiling(loop.data, slope, intercept)
  effect    <- ceiling / loop.data$scope
  ineffs    <- p_ineffs(loop.data, intercept, slope, ceiling)
  above     <- p_above(loop.data, slope, intercept)
  
  return(list(line=qr,
              ceiling=ceiling, slope=slope, effect=effect,
              intercept=intercept, above=above, ineffs=ineffs,
              bottleneck=NULL))
}