p_nca_sfa <-
function (loop.data, bn.data) {
  x <- loop.data$x
  y <- loop.data$y

  sfa <- NULL
  options(warn=-1)
  type <- ifelse(loop.data$flip.y, "cost", "production")
  try(sfa <- sfa(y~x, form=type, silent=TRUE))
  options(warn=0)
  if (is.null(sfa)) {
    message()
    message("Ignoring SFA due to errors !")
    message()

    return(list(line=NULL, ceiling=NA, slope=NA, effect=NA,
              intercept=NA, above=NA, ineffs=list(x=NA, y=NA, abs=NA, rel=NA),
              bottleneck=NULL))
  }

  intercept   <- unname(coef(sfa)["Intercept"])
  slope       <- unname(coef(sfa)["x"])
  ceiling     <- p_ceiling(loop.data, slope, intercept)
  effect      <- ceiling / loop.data$scope.area
  ineffs      <- p_ineffs(loop.data, slope, intercept)
  above       <- p_above(loop.data, slope, intercept)
  bottleneck  <- p_bottleneck(loop.data, bn.data, slope, intercept)

  sfa$coef    <- sfa$coef[c(-3, -4)]

  return(list(line=sfa,
              slope=slope, intercept=intercept,
              ceiling=ceiling, effect=effect,
              ineffs=ineffs, above=above,
              bottleneck=bottleneck))
}