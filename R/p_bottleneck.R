# 0 : NN / NA
# 1 : NN / highest observed values
# 2 : calculated values

p_bottleneck <-
function (loop.data, bn.data, slope, intercept) {
  if (is.null(bn.data)) {
    return ( NULL )
  }

  theo        <- loop.data$scope.theo
  flip.x      <- loop.data$flip.x
  precision.x <- ifelse(bn.data$bn.x.id %in% c(1, 2, 4), 1, 3)

  if (is.na(intercept) || is.na(slope)) {
    mpx <- p_mpx_single_peer(bn.data, theo, flip.x)
  } else {
    mpx <- (bn.data$mpy - intercept) / slope
    mpx <- p_edge_cases(mpx, bn.data, theo, flip.x)
  }

  nn.value <- p_nn_value(mpx, loop.data, bn.data)
  na.value <- p_na_value(mpx, loop.data, bn.data)

  # Display Xs as percentage (either cutoff or 0-high) or percentile
  mpx <- p_transform_mpx(loop.data, mpx, bn.data$bn.x.id)

  return( p_pretty_mpx(loop.data, mpx, nn.value, na.value, precision.x) )
}

p_bottleneck_ce <-
function (loop.data, bn.data, peers, type) {
  if (is.null(bn.data)) {
    return ( NULL )
  }

  theo        <- loop.data$scope.theo
  flip.x      <- loop.data$flip.x
  flip.y      <- loop.data$flip.y
  precision.x <- ifelse(bn.data$bn.x.id %in% c(1, 2, 4), 1, 3)

  if (type == "fdh") {
    mpx <- p_bottleneck_fdh(bn.data, peers, theo, flip.x, flip.y)
  } else if (type == "vrs") {
    mpx <- p_bottleneck_vrs(bn.data, peers, theo, flip.x, flip.y)
  }

  nn.value <- p_nn_value(mpx, loop.data, bn.data)
  na.value <- p_na_value(mpx, loop.data, bn.data)

  # Display Xs as percentage (either cutoff or 0-high) or percentile
  mpx <- p_transform_mpx(loop.data, mpx, bn.data$bn.x.id)

  return( p_pretty_mpx(loop.data, mpx, nn.value, na.value, precision.x) )
}

p_bottleneck_fdh <-
function (bn.data, peers, theo, flip.x, flip.y) {
  mpy <- bn.data$mpy
  mpx <- matrix(nrow=length(mpy), ncol=1)
  x.peers <- peers[,1]
  y.peers <- peers[,2]

  for (j in 1:length(mpy)) {
    # search the peer that is closest above to the desired outcome,
    # and select it corresponding x value of that peer
    if (flip.y) {
      index <- which(y.peers < (mpy[j,1] + epsilon))[1]
    } else {
      index <- which(y.peers > (mpy[j,1] - epsilon))[1]
    }

    if (is.na(index)) {
      mpx[j,1]  <- NA
    } else {
      mpx[j,1]  <- x.peers[index]
    }
  }

  mpx <- p_edge_cases(mpx, bn.data, theo, flip.x)

  return( mpx )
}

p_bottleneck_vrs <-
function (bn.data, peers, theo, flip.x, flip.y) {
  mpy <- bn.data$mpy
  mpx <- matrix(nrow=length(mpy), ncol=1)
  x.peers <- peers[,1]
  y.peers <- peers[,2]

  calculate_x <- function(y, peers, index) {
    p1 <- peers[index - 1, ]
    p2 <- peers[index,     ]
    return(p1[1] + (y - p1[2]) * (p1[1] - p2[1]) / (p1[2] - p2[2]))
  }

  for (j in 1:length(mpy)) {
    if (flip.y) {
      index <- which(y.peers < (mpy[j,1] + epsilon))[1]
    } else {
      index <- which(y.peers > (mpy[j,1] - epsilon))[1]
    }

    if (is.na(index)) {
      mpx[j,1] <- NA
    } else if (index == 1) {
      mpx[j,1] <- x.peers[index]
    } else {
      mpx[j, 1] <- calculate_x(mpy[j,1], peers, index)
    }
  }

  mpx <- p_edge_cases(mpx, bn.data, theo, flip.x)

  return( mpx )
}

p_mpx_single_peer <-
function (bn.data, theo, flip.x) {
  mpy <- bn.data$mpy
  cutoff <- bn.data$cutoff

  # if peers is a vector there is only one peer
  if (cutoff == 0) {
    mpx <- matrix(Inf, nrow=length(mpy), ncol=1)
  } else {
    mpx <- matrix(theo[1 + flip.x], nrow=length(mpy), ncol=1)
  }
  return( mpx )
}

p_transform_mpx <-
function (loop.data, mpx, bn.x.id) {
  flip.x <- loop.data$flip.x
  theo <- loop.data$scope.theo

  # Display Xs as percentage (either cutoff or 0-high) or percentile
  if (bn.x.id == 1) {
    mpx <- 100 * (mpx - theo[1]) / (theo[2] - theo[1])
  } else if (bn.x.id == 2) {
    mpx <- 100 * mpx / theo[2]
  } else if (bn.x.id == 4) {
    if (flip.x) {
      percentile <- ecdf(sort(-loop.data$x))
      tmp <- -mpx - epsilon
    }
    else {
      percentile <- ecdf(sort(loop.data$x))
      tmp <- mpx - epsilon
    }
    mpx <- matrix(100 * percentile(tmp), ncol = 1)
  }

  return( mpx )
}

p_nn_value <-
function (mpx, loop.data, bn.data) {
  if (bn.data$cutoff %in% c(0, 1)) {
    return("NN")
  }

  flip.x <- loop.data$flip.x
  theo <- loop.data$scope.emp
  nn.value <- p_if_min_else_max(!flip.x, mpx, na.rm=TRUE)

  return( p_transform_value(loop.data, nn.value, theo, bn.data$bn.x.id) )
}

p_na_value <-
function (mpx, loop.data, bn.data) {
  if (bn.data$cutoff == 0) {
    return( "NA" )
  }

  flip.x <- loop.data$flip.x
  theo <- loop.data$scope.theo
  na.value <- p_if_min_else_max(flip.x, mpx, na.rm=TRUE)

  return( p_transform_value(loop.data, na.value, theo, bn.data$bn.x.id) )
}

p_transform_value <-
function (loop.data, value, theo, bn.x.id) {
  if (bn.x.id == 1) {
    value <- 100 * (value - theo[1]) / (theo[2] - theo[1])
  } else if (bn.x.id == 2) {
    value <- 100 * value / theo[2]
  } else if (bn.x.id == 4) {
    percentile <- ecdf(sort(loop.data$x))
    value <- 100 * percentile(value)
  }

  return( value )
}

p_pretty_mpx <-
function (loop.data, mpx, nn.value, na.value, precision.x) {
  subst <- function (x) {
    if (is.na(x)) {
      return( p_pretty_number(na.value, na.value, prec=precision.x) )
    } else if (is.infinite(x)) {
      if (is.infinite(nn.value)) { nn.value <- "NN" }
      return( p_pretty_number(nn.value, nn.value, prec=precision.x) )
    } else {
      return( p_pretty_number(x, prec=precision.x) )
    }
  }

  mpx <- matrix(mapply(subst, mpx), ncol=1)
  colnames(mpx) <- c(loop.data$names[loop.data$idx])

  return( mpx )
}

p_edge_cases <-
function (mpx, bn.data, theo, flip.x) {
  mpx[mpx < (theo[1] + epsilon)] <- ifelse(flip.x, NA, -Inf)
  if (bn.data$cutoff == 0) {
    mpx[mpx > (theo[2] - epsilon)] <- ifelse(flip.x, Inf, NA)
  } else if (bn.data$cutoff == 1) {
    mpx[mpx > (theo[2] - epsilon)] <- theo[2]
  }

  return(mpx)
}
