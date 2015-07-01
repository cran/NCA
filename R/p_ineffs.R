p_ineffs <-
function (loop.data, intercept, slope, ceiling) {
  if (is.na(slope) || slope <= 0) {
    return( list(x=NA, y=NA, abs=NA, rel=NA) )
  }
  
  x.c     <- (loop.data$y.high - intercept) / slope
  x.c.max <- min(loop.data$x.high, x.c)
  y.c.min <- slope * loop.data$x.low + intercept
  
  ineffs.x    <- 100 * (loop.data$x.high - x.c.max) / (loop.data$x.high - loop.data$x.low)
  ineffs.y    <- 100 * (y.c.min - loop.data$y.low)  / (loop.data$y.high - loop.data$y.low)
  ineffs.abs  <- loop.data$scope - 2 * ceiling
  ineffs.rel  <- 100 * ineffs.abs / loop.data$scope
  
  return( list(x=ineffs.x, y=unname(ineffs.y), abs=ineffs.abs, rel=ineffs.rel) )
}

p_ineffs_ce <-
function (loop.data, peers, ceiling) {
  # if there is only one peer, the ceiling zone is zero
  if (is.vector(peers)) {
    return( list(x=NA, y=NA, abs=NA, rel=NA) )
  }

  x.c.max <- tail(peers, n=1)[3]
  y.c.min <- peers[1,4]

  ineffs.x    <- 100 * (loop.data$x.high - x.c.max) / (loop.data$x.high - loop.data$x.low)
  ineffs.y    <- 100 * (y.c.min - loop.data$y.low)  / (loop.data$y.high - loop.data$y.low)
  ineffs.abs  <- loop.data$scope - 2 * ceiling
  ineffs.rel  <- 100 * ineffs.abs / loop.data$scope
  
  return( list(x=ineffs.x, y=unname(ineffs.y), abs=ineffs.abs, rel=ineffs.rel) )
}