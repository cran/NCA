p_nca_lh <-
function (loop.data, bn.data) {
  peers <- p_peers(loop.data)

  if (!is.vector(peers) && length(peers) > 2) {
    # Get coordinates for first and last peer
    x1 <- head(peers, n=1)[1]
    x2 <- tail(peers, n=1)[1]
    y1 <- head(peers, n=1)[2]
    y2 <- tail(peers, n=1)[2]

    line      <- list(c(x1, x2), c(y1, y2))
    slope     <- (y2 - y1) / (x2 - x1)
    intercept <- y2 - (slope * x2)
    ceiling   <- p_ceiling(loop.data, slope, intercept)
    effect    <- ceiling / loop.data$scope.area
    ineffs    <- p_ineffs(loop.data, slope, intercept)
    above     <- p_above(loop.data, slope, intercept)
  } else {
    line      <- NULL
    slope     <- NA
    intercept <- NA
    ceiling   <- 0
    effect    <- 0
    ineffs    <- list(x=NA, y=NA, abs=NA, rel=NA)
    above     <- NA    
  }
  
  bottleneck  <- p_bottleneck(loop.data, bn.data, slope, intercept)

  return(list(line=line,
              slope=slope, intercept=intercept,
              ceiling=ceiling, effect=effect,
              ineffs=ineffs, above=above,
              bottleneck=bottleneck))
}