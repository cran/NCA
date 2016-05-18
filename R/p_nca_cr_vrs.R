p_nca_cr_vrs <-
function (loop.data, bn.data) {
  x <- loop.data$x
  y <- loop.data$y
  flip.x <- loop.data$flip.x
  flip.y <- loop.data$flip.y
  weighting <- loop.data$weighting

  # Find the points on the ceiling ("PEERS")
  peers <- p_peers(loop.data, TRUE)

  if (!is.vector(peers) && length(peers) > 2) {
    # Get the weights if resuired
    weights <- NULL
    if (weighting) {
      weights <- p_weights(loop.data, peers)
    }

    # Perform OLS through the peers
    x <- peers[,1]
    y <- peers[,2]
    line <- lm(y~x, weights=weights)
    
    intercept <- unname(coef(line)["(Intercept)"])
    slope     <- unname(coef(line)["x"])
    ceiling   <- p_ceiling(loop.data, slope, intercept)
    above     <- p_above(loop.data, slope, intercept)
  } else {
    ceiling   <- 0
    intercept <- NA
    slope     <- NA
    line      <- NULL
    above     <- 0
  }
  
  effect      <- ceiling / loop.data$scope.area
  ineffs      <- p_ineffs(loop.data, slope, intercept)
  bottleneck  <- p_bottleneck(loop.data, bn.data, slope, intercept)
  
  return(list(line=line,
              slope=slope, intercept=intercept,
              ceiling=ceiling, effect=effect,
              ineffs=ineffs, above=above,
              bottleneck=bottleneck))
}