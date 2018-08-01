p_nca_cr_cm <-
function (loop.data, bn.data) {
  x <- loop.data$x
  y <- loop.data$y
  weighting <- loop.data$weighting

  columns     <- p_columns(loop.data, FALSE)
  cm_peers    <- t(columns[c(2, 5), ])

  if (!is.vector(cm_peers) && length(cm_peers) > 2) {
    # Get the weights if required
    weights <- if (weighting) columns[1,]

    # Perform OLS through the peers
    x <- cm_peers[,1]
    y <- cm_peers[,2]
    line <- lm(y~x, weights=weights)
    
    intercept <- unname(coef(line)["(Intercept)"])
    slope     <- unname(coef(line)["x"])
    ceiling   <- p_ceiling(loop.data, slope, intercept)
    above     <- p_above(loop.data, slope, intercept)
  } else {
    line      <- NULL
    ceiling   <- 0
    intercept <- NA
    slope     <- NA
    above     <- 0
  }

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