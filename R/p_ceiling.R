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
  p.l <- c(theo[1], slope * theo[1] + intercept)
  p.r <- c(theo[2], slope * theo[2] + intercept)

  # Line completly outside scope
  if (p.l[2] > theo[4] && p.r[2] > theo[4]) {
    return( ifelse(!flip.y, NaN, loop.data$scope.area) )
  } else if (p.l[2] < theo[3] && p.r[2] < theo[3]) {
    return( ifelse(!flip.y, loop.data$scope.area, NaN) )
  }

  if (slope > 0) {
    # Line doesn't go through the left boundary but lower boundry
    if (p.l[2] < theo[3]) {
      p.l <- c((theo[3] - intercept) / slope, theo[3])
    }

    # Line doesn't go through the right boundary but upper boundry
    if (p.r[2] > theo[4]) {
      p.r <- c((theo[4] - intercept) / slope, theo[4])
    }

    area_above <- 0.5 * (p.r[1] - p.l[1]) * (p.r[2] - p.l[2])
    area_above <- area_above + (theo[2] - theo[1]) * (theo[4] - p.r[2])
    area_above <- area_above + (p.l[1] - theo[1]) * (theo[4] - theo[3])
    area_above <- area_above - (p.l[1] - theo[1]) * (theo[4] - p.r[2])
  } else if (slope < 0) {
    # Nope, line goes through upper boundary
    if (p.l[2] > theo[4]) {
      p.l <- c((theo[4] - intercept) / slope, theo[4])
    }

    # Line doesn't go through lower boundary but right boundary
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
function (loop.data, peers, method) {
  emp <- loop.data$scope.emp
  theo <- loop.data$scope.theo
  flip.x <- loop.data$flip.x
  flip.y <- loop.data$flip.y

  # Get the part of the ceiling between theo and emp scope
  ceiling <- p_scope_ceiling(peers, theo, emp, flip.x, flip.y)

  # Single peer, different cases for FHD+VRS and conf lines
  if (nrow(unique(peers)) <= 1) {
    # if there is only one peer, the ceiling zone is zero or 'scope ceiling'
    if (method %in% c("fdh", "vrs")) {
      return ( ceiling )
    } else {
      stop("method not allowed")
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
    } else {
      stop("method not allowed")
    }
  }

  return( ceiling )
}

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
  lower   <- (theo[2] - theo[1]) * y1
  upper   <- (theo[2] - theo[1]) * y2

  if (!flip.x && !flip.y) {
    # Upper left corner
    cross <- x1 * y2
    return (left + upper - cross)
  } else if (flip.x && !flip.y) {
    # Upper right corner
    cross <- x2 * y2
    return (right + upper - cross)
  } else if (flip.x && flip.y) {
    # Lower right corner
    cross <- x2 * y1
    return (right + lower - cross)
  } else if (!flip.x && flip.y) {
    # Lower left corner
    cross <- x1 * y1
    return (left + lower - cross)
  }
}

# TODO Is this the correct place?
p_nca_wrapper <-
function (ceiling, loop.data, bn.data, effect_aggregation) {
  analysis <- do.call(paste0("p_nca_", ceiling), list(loop.data, bn.data))

  loop.data$flip.x  <- !loop.data$flip.x
  if (2 %in% effect_aggregation) {
    tmp_analysis <- do.call(paste0("p_nca_", ceiling), list(loop.data, bn.data))
    analysis$effect <- analysis$effect + tmp_analysis$effect
  }

  loop.data$flip.y  <- !loop.data$flip.y
  if (4 %in% effect_aggregation) {
    tmp_analysis <- do.call(paste0("p_nca_", ceiling), list(loop.data, bn.data))
    analysis$effect <- analysis$effect + tmp_analysis$effect
  }

  loop.data$flip.x  <- !loop.data$flip.x
  if (3 %in% effect_aggregation) {
    tmp_analysis <- do.call(paste0("p_nca_", ceiling), list(loop.data, bn.data))
    analysis$effect <- analysis$effect + tmp_analysis$effect
  }

  return ( analysis )
}
