p_peers <-
function (loop.data, vrs=FALSE) {
  x <- loop.data$x
  y <- loop.data$y
  flip.x <- loop.data$flip.x
  flip.y <- loop.data$flip.y

  if (length(x) < 2) {
    return(NULL)
  }

  x.sorted <- x[order(if (flip.x) -x else x)]
  y.sorted <- y[order(if (flip.x) -x else x)]

  x.min <- x.sorted[1]
  y.min <- y.sorted[1]
  peers <- list(x.min, y.min)

  for (i in 2:length(x.sorted)) {
    choose.peer <- ifelse(flip.y, y.sorted[i] < y.min, y.sorted[i] > y.min)
    if (choose.peer) {
      x.min <- x.sorted[i]
      y.min <- y.sorted[i]

      # If new X is equal to last peers X, replace last peers Y
      if (x.min == peers[length(peers) - 1]) {
        peers <- head(peers, -2)
      }

      # Remove 'invalid' if VRS
      if (vrs) {
        while ( p_invalid_peers (peers, x.min, y.min, flip.x, flip.y) ) {
          peers <- head(peers, -2)
        }
      }

      peers <- c(peers, x.min, y.min)
    }
  }

  peers <- as.numeric(peers)
  return (t ( matrix(peers, 2) ))
}

p_invalid_peers <-
function (peers, x3, y3, flip.x, flip.y) {
  if (length(peers) < 4) {
    return (FALSE)
  }

  tmp <- as.numeric( tail( peers, 4) )
  x1 <- tmp[1]
  y1 <- tmp[2]
  x2 <- tmp[3]
  y2 <- tmp[4]

  slope0 <- (y2 - y1) / (x2 - x1)
  slope1 <- (y3 - y2) / (x3 - x2)

  if ( (!flip.x && !flip.y) || (flip.x && flip.y) ) {
    return (slope0 <= slope1)
  } else {
    return (slope0 >= slope1)
  }
}
