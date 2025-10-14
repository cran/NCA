p_peers <-
  function (loop.data, vrs = FALSE) {
    x <- loop.data$x
    y <- loop.data$y
    flip.x <- loop.data$flip.x
    flip.y <- loop.data$flip.y

    if (length(x) < 2) {
      return(NULL)
    }

    x.sorted <- x[order(if (flip.x) -x else x, if (flip.y) -y else y)]
    y.sorted <- y[order(if (flip.x) -x else x, if (flip.y) -y else y)]
    peers <- matrix(c(x.sorted[1], y.sorted[1]), ncol = 2)
    rownames.org <- rownames(x)[order(if (flip.x) -x else x)]
    rownames <- rownames.org[1]

    for (i in 2:length(x.sorted)) {
      x.curr <- x.sorted[i]
      y.curr <- y.sorted[i]
      x.prev <- tail(peers, n = 1)[1]
      y.prev <- tail(peers, n = 1)[2]
      x.equal <- p_is_equal(x.prev, x.curr)
      y.equal <- p_is_equal(y.prev, y.curr)

      # Choose this peer if the Y value is larger than previous (dep. on flipping)
      next.peer <- ifelse(flip.y, y.curr < y.prev, y.curr > y.prev)
      # Also choose this peer if it is a duplicate
      if (next.peer || (x.equal && y.equal)) {
        # If X of this peer is equal to X of last peer(s), replace last peer(s)
        while (x.equal && !y.equal) {
          peers <- head(peers, -1)
          rownames <- head(rownames, -1)
          if (!length(peers)) {
            break
          }
          x.prev <- tail(peers, n = 1)[1]
          y.prev <- tail(peers, n = 1)[2]
          x.equal <- p_is_equal(x.prev, x.curr)
          y.equal <- p_is_equal(y.prev, y.curr)
        }

        # Remove 'invalid' if VRS
        if (vrs) {
          while (p_invalid_peers(peers, x.curr, y.curr, flip.x, flip.y)) {
            peers <- head(peers, -1)
            rownames <- head(rownames, -1)
          }
        }

        peers <- rbind(peers, c(x.curr, y.curr))
        rownames <- c(rownames, rownames.org[i])
      }
    }

    rownames(peers) <- rownames
    colnames(peers) <- c("X", "Y")

    return(peers)
  }

p_invalid_peers <-
  function (peers, x3, y3, flip.x, flip.y) {
    if (nrow(peers) < 2) {
      return(FALSE)
    }

    # Take x and y from last 2 rows
    tmp <- tail(peers, 2)
    x1 <- tmp[1, 1]; y1 <- tmp[1, 2]
    x2 <- tmp[2, 1]; y2 <- tmp[2, 2]

    # Last 2 rows equal or last row and new row equal
    if ((p_is_equal(x1, x2) && p_is_equal(y1, y2)) ||
      (p_is_equal(x2, x3) && p_is_equal(y2, y3))) {
      return(FALSE)
    }

    slope0 <- (y2 - y1) / (x2 - x1)
    slope1 <- (y3 - y2) / (x3 - x2)

    if (flip.x == flip.y) {
      return(slope0 <= slope1)
    } else {
      return(slope0 >= slope1)
    }
  }

p_aggregate_peers <-
  function (model.peers, x) {
    # Aggregate all ceiling peers for independent variable x
    peers <- NULL
    for (ceiling in names(model.peers)) {
      peers <- rbind(peers, model.peers[[ceiling]][[x]])
    }
    return(unique(peers))
  }

p_get_line_peers <-
  function (loop.data, intercept, slope) {
    peers <- p_peers(loop.data, vrs = TRUE)
    if (is.null(intercept) || is.null(slope)) {
      return(NULL)
    }
    return(p_best_peers(peers, intercept, slope))
  }

p_best_peers <-
  function (peers, intercept, slope) {
    df <- cbind(peers, abs(intercept + slope * peers[, 1] - peers[, 2]))
    df <- df[order(df[, 3]),]
    delta <- abs(df[1, 2] / 1e6)
    peers <- subset(df[, c(1, 2)], abs(df[, 3]) < delta)
    return(peers)
  }
