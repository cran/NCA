nca_sfa <-
function (loop.data, mpy, cutoff, bottleneck.x) {
  x           <- loop.data$x
  y           <- loop.data$y  
  sfa         <- sfa(y~x, form="production")
  intercept   <- unname(coef(sfa)["Intercept"])
  slope       <- unname(coef(sfa)["x"])
  ceiling     <- p_ceiling(loop.data, slope, intercept)
  effect      <- ceiling / loop.data$scope
  ineffs      <- p_ineffs(loop.data, intercept, slope, ceiling)
  above       <- p_above(loop.data, slope, intercept)
  bottleneck  <- p_bottleneck(loop.data, mpy, slope, intercept, cutoff, bottleneck.x)
  
  return(list(line=sfa[c(1, 2)],
              ceiling=ceiling, slope=slope, effect=effect,
              intercept=intercept, above=above, ineffs=ineffs,
              bottleneck=bottleneck))
}