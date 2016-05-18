p_ceiling <-
function (loop.data, slope, intercept) {
  flip.x <- loop.data$flip.x
  flip.y <- loop.data$flip.y

  if (is.na(slope) || is.na(intercept)) {
    return( NaN )
  }

  # Upper left and lower right
  if ( flip.x == flip.y && slope < 0) {
    return( NaN )
  }
  # Lower left and upper right
  if ( flip.x != flip.y && slope > 0) {
    return( NaN )
  }

  # Get the intersections of the line and theo[1] / theo[2]
  theo <- loop.data$scope.theo
  p.l <- c(theo[1], slope * loop.data$scope.theo[1] + intercept)
  p.r <- c(theo[2], slope * loop.data$scope.theo[2] + intercept)

  # Line completly outside scope
  if (p.l[2] > theo[4] && p.r[2] > theo[4]) {
    return( ifelse(!flip.y, NaN, loop.data$scope.area) )
  } else if (p.l[2] < theo[3] && p.r[2] < theo[3]) {
    return( ifelse(!flip.y, loop.data$scope.area, NaN) )
  }

  if (slope > 0) {
    # Line doesn't go through the left boundary but bottom boundry
    if (p.l[2] < theo[3]) {
      p.l <- c((theo[3] - intercept) / slope, theo[3])
    }

    # Line doesn't go through the right boundary but top boundry
    if (p.r[2] > theo[4]) {
      p.r <- c((theo[4] - intercept) / slope, theo[4])
    }

    area_above <- 0.5 * (p.r[1] - p.l[1]) * (p.r[2] - p.l[2])
    area_above <- area_above + (theo[2] - theo[1]) * (theo[4] - p.r[2])
    area_above <- area_above + (p.l[1] - theo[1]) * (theo[4] - theo[3])
    area_above <- area_above - (p.l[1] - theo[1]) * (theo[4] - p.r[2])
  } else if (slope < 0) {
    # Nope, line goes through top boundary
    if (p.l[2] > theo[4]) {
      p.l <- c((theo[4] - intercept) / slope, theo[4])
    }

    # Line doesn't go through bottom boundary but right boundary
    if (p.r[2] < theo[3]) {
      p.r <- c((theo[3] - intercept) / slope, theo[3])
    }

    area_above <- 0.5 * (p.r[1] - p.l[1]) * (p.l[2] - p.r[2])
    area_above <- area_above + (theo[2] - theo[1]) * (theo[4] - p.l[2])
    area_above <- area_above + (theo[2] - p.r[1]) * (theo[4] - theo[3])
    area_above <- area_above - (theo[2] - p.r[1]) * (theo[4] - p.l[2])
  } else if (slope == 0) {
    area_above <- (theo[2] - theo[1]) * (theo[4] - intercept)
  }

  return( ifelse(!flip.y, area_above, loop.data$scope.area - area_above) )
}

p_ce_ceiling <-
function (loop.data, peers, flip.x, flip.y, method) {
  emp <- loop.data$scope.emp
  theo <- loop.data$scope.theo
  flip.x <- loop.data$flip.x
  flip.y <- loop.data$flip.y

  # Get the part of the ceiling between theo and emp scope
  ceiling <- p_scope_ceiling(peers, theo, emp, flip.x, flip.y)

  # Single peer, different cases for FHD+VRS
  if (is.vector(peers) || length(peers) <= 2) {
    # if there is only one peer, the ceiling zone is zero or 'scope ceiling'
    if (method %in% c("fdh", "vrs")) {
      return ( ceiling )
    }
  }

  for (i in 1:(nrow(peers)-1)) {
    if (method == "fdh") {
      x.length <- peers[i+1,1] - emp[1 + flip.x]
      y.length <- peers[i+1,2] - peers[i,2]
      ceiling <- ceiling + abs(x.length * y.length)
    } else if (method == "vrs") {
      part.a <- (peers[i+1,2] - peers[i,2]) * (peers[i+1,1] - peers[1,1])
      part.b <- 0.5 * (peers[i+1,2] - peers[i,2]) * (peers[i+1,1] - peers[i,1])
      ceiling <- ceiling + abs(part.a) - abs(part.b)
    }
  }

  return( ceiling )
}

#
p_scope_ceiling <-
function (peers, theo, emp, flip.x, flip.y) {
  if (identical(emp, theo)) {
    return(0)
  }

  x1 <- emp[1] - theo[1]
  x2 <- theo[2] - emp[2]
  y1 <- emp[3] - theo[3]
  y2 <- theo[4] - emp[4]

  left    <- x1 * (theo[4] - theo[3])
  right   <- x2 * (theo[4] - theo[3])
  bottom  <- (theo[2] - theo[1]) * y1
  top     <- (theo[2] - theo[1]) * y2

  if (!flip.x && !flip.y) {
    # Upper left corner
    cross <- x1 * y2
    return (left + top - cross)
  } else if (flip.x && !flip.y) {
    # Upper right corner
    cross <- x2 * y2
    return (right + top - cross)
  } else if (flip.x && flip.y) {
    # Lower right corner
    cross <- x2 * y1
    return (right + bottom - cross)
  } else if (!flip.x && flip.y) {
    # Lower left corner
    cross <- x1 * y1
    return (left + bottom - cross)
  }
}