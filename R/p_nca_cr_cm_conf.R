p_nca_cr_cm_conf <-
function (loop.data, bn.data) {
  if (is.null(loop.data$ce_cm_conf_columns)) {
    columns <- p_columns(loop.data, TRUE)
  } else {
    columns <- loop.data$ce_cm_conf_columns
  }

  if (ncol(columns) > 1) {
    x     <- columns[4,]
    y     <- columns[5,]
    line  <- lm(y~x, weights=columns[1,])
  } else {
    x     <- c(loop.data$scope.theo[1], loop.data$scope.theo[2])
    y     <- c(columns[5, 1], columns[5, 1])
    line  <- lm(y~x)
  }

  # Needed for p_con_lim
  attr(line, "columns") <- columns

  intercept   <- unname(coef(line)["(Intercept)"])
  slope       <- unname(coef(line)["x"])
  ceiling     <- p_ceiling(loop.data, slope, intercept)
  above       <- p_above(loop.data, slope, intercept)
  effect      <- ceiling / loop.data$scope.area
  accuracy    <- p_accuracy(loop.data, above)
  fit         <- get_fit(ceiling, loop.data$ce_fdh_ceiling)
  ineffs      <- p_ineffs(loop.data, slope, intercept)
  bottleneck  <- p_bottleneck(loop.data, bn.data, slope, intercept)

  return(list(line=line,
              slope=slope, intercept=intercept,
              ceiling=ceiling, effect=effect,
              above=above, accuracy=accuracy, fit=fit,
              ineffs=ineffs, bottleneck=bottleneck))
}