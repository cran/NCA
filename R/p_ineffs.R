p_ineffs <-
function (loop.data, slope, intercept) {
  flip.x <- loop.data$flip.x
  flip.y <- loop.data$flip.y

  # Upper left and lower right
  if ( (flip.x == flip.y) && (is.na(slope) || slope <= 1E-3) ) {
    return( list(x=NA, y=NA, abs=NA, rel=NA) )
  }
  # Lower left and upper right
  if ( (flip.x != flip.y) && (is.na(slope) || slope >= -1E-3) ) {
    return( list(x=NA, y=NA, abs=NA, rel=NA) )
  }

  # Get the x-value of the line crossing the upper or low boundry
  y.xlim <- loop.data$scope.theo[4 - flip.y]
  x.lim <- (y.xlim - intercept) / slope

  # Get the y-value of the line crossing the left or right boundry
  x.ylim <- loop.data$scope.theo[1 + flip.x]
  y.lim <- slope * x.ylim + intercept

  return ( p_ineff(loop.data, x.lim, y.lim) )
}

p_ineffs_ce <-
function (loop.data, peers) {
  # if there is only one peer, the ceiling zone is zero
  if (is.vector(peers) || length(peers) == 2) {
    return( list(x=NA, y=NA, abs=NA, rel=NA) )
  }

  # Get the x-value of the line crossing the upper or low boundry
  x.lim <- tail(peers, n=1)[1]

  # Get the y-value of the line crossing the left or right boundry
  y.lim <- head(peers, n=1)[2]

  return ( p_ineff(loop.data, x.lim, y.lim) )
}

p_ineff <-
function (loop.data, x.lim, y.lim) {
  flip.x <- loop.data$flip.x
  flip.y <- loop.data$flip.y

  if (flip.x) {
    x.lim <- max(loop.data$scope.theo[1], x.lim)
    x.eff <- x.lim - loop.data$scope.theo[1]
  } else {
    x.lim <- min(loop.data$scope.theo[2], x.lim)
    x.eff <- loop.data$scope.theo[2] - x.lim
  }

  if (flip.y) {
    y.lim <- min(loop.data$scope.theo[4], y.lim)
    y.eff <- loop.data$scope.theo[4] - y.lim
  } else {
    y.lim <- max(loop.data$scope.theo[3], y.lim)
    y.eff <- y.lim - loop.data$scope.theo[3]
  }

  ineffs.x    <- x.eff / (loop.data$scope.theo[2] - loop.data$scope.theo[1])
  ineffs.y    <- y.eff / (loop.data$scope.theo[4] - loop.data$scope.theo[3])
  ineffs.rel  <- ineffs.x + ineffs.y - ineffs.x * ineffs.y
  ineffs.abs  <- loop.data$scope.area * ineffs.rel

  return( list(x=ineffs.x * 100, y=unname(ineffs.y) * 100,
               abs=ineffs.abs, rel=ineffs.rel * 100) )
}