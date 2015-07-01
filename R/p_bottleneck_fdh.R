p_bottleneck_fdh <-
function (loop.data, mpy, peers, cutoff, bottleneck.x) {
  bottleneck.x <- p_validate_bottleneck(bottleneck.x, "x")

  if (is.vector(peers)) {
    x.peers <- peers[3]
    mpx <- matrix(, nrow=length(mpy), ncol=1)
    
    # start one peer; if peers is a vector there is only one peer
    for (j in 1:length(mpy)) {
      options(warn=-1)
      x.peers[j] <- x.peers
      options(warn=0)
      
      if (cutoff == 0) {
        x.peers[j] <- NA
      } else if (cutoff == 1) {
        x.peers[j] <- loop.data$x.low
      }

      # Display Xs as percentage, either cutoff or 0-high
      if (p_bottleneck_id(bottleneck.x) == 1) {
        x.peers[j] <- 100 * (x.peers[j] - loop.data$x.low) / (loop.data$x.high - loop.data$x.low)
      } else if (p_bottleneck_id(bottleneck.x) == 2) {
        x.peers[j] <- 100 * x.peers[j] / loop.data$x.high
      }
      mpx[j, 1]  <- as.matrix(x.peers[j])
    }
  } else {
    x.peers <- peers[,3]
    y.peers <- peers[,4]
    mpx <- matrix(nrow=length(mpy), ncol=1)
    
    for (j in 1:length(mpy)) {
      # search the peer that is closest above to the desired outcome,
      # and select it corresponding x value of that peer
      g1 <- c()
      for (i in 1:length(y.peers)) {
        g1[i] <- y.peers[i] - mpy[j,1]
      }
      g1[j] <- min(g1[g1>=0], na.rm=TRUE)

      for (i in 1:length(y.peers)) {
        if ((y.peers[i] - mpy[j,1] - g1[j]) == 0) {
          mpx[j,1]  <- as.matrix(x.peers[i])
        }
      }
    }
    
    if (cutoff == 0) {
      mpx [mpx <= loop.data$x.low]  <- NA
      # better for later: make sure that if desired Y equals Y of lowest peer
      # the values should be given, not NA;
      # hence change <= in to < and add a condition)
      mpx [mpx >  loop.data$x.high] <- NA
    } else if (cutoff == 1) {
      mpx [mpx <= loop.data$x.low]  <- loop.data$x.low
      mpx [mpx >  loop.data$x.high] <- loop.data$x.high
    }

    if (p_bottleneck_id(bottleneck.x) == 3) {
      max.value <- loop.data$x.high
    } else {
      max.value <- 100
    }

    # Display Xs as percentage (either cutoff or 0-high) or percentile
    if (p_bottleneck_id(bottleneck.x) == 1) {
      mpx <- 100 * (mpx - loop.data$x.low) / (loop.data$x.high - loop.data$x.low)
    } else if (p_bottleneck_id(bottleneck.x) == 1) {
      mpx <- 100 * mpx / loop.data$x.high
    } else if (p_bottleneck_id(bottleneck.x) == 4) {
      percentile <- ecdf(sort(loop.data$x))
      mpx <- matrix(100 * percentile(mpx), ncol=1)
    }
  }

  return (p_pretty_mpx(loop.data, mpx, max.value))
}