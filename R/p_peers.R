p_peers <-
function (loop.data, vrs=FALSE, trend=FALSE, inflate=FALSE, limit=FALSE) {
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
  rownames.org <- rownames(x)[order(if (flip.x) -x else x)]
  rownames <- rownames.org[1]

  for (i in 2:length(x.sorted)) {
    choose.peer <- ifelse(flip.y, y.sorted[i] < y.min, y.sorted[i] > y.min)
    if (choose.peer) {
      x.min <- x.sorted[i]
      y.min <- y.sorted[i]

      # If new X is equal to last peers X, replace last peers Y
      max.diff <- min(abs(x.min), abs(peers[length(peers) - 1][[1]])) / 1e6
      diff <- abs(x.min - peers[length(peers) - 1][[1]])
      if (diff <= max.diff) {
        peers <- head(peers, -2)
        rownames <- head(rownames, -1)
      }

      # Remove 'invalid' if VRS
      if (vrs) {
        while ( p_invalid_peers (peers, x.min, y.min, flip.x, flip.y) ) {
          peers <- head(peers, -2)
          rownames <- head(rownames, -1)
        }
      }

      peers <- c(peers, x.min, y.min)
      rownames <- c(rownames, rownames.org[i])
    }
  }

  # Add 'lower right' corners to peer matrix
  # TODO Check rownames, only called from p_nca_ct_fdh
  #if (trend && length(peers) > 2) {
  #  # TODO Check rownames
  #  tmp <- c()
  #  for (i in seq(1, length(peers) - 2, by=2)) {
  #    tmp <- c(tmp, peers[i], peers[i+1], peers[i+2], peers[i+1])
  #  }
  #  tmp <- c(tmp, peers[i+2], peers[i+3])
  #  peers <- tmp
  #}

  peers <- as.numeric(peers)
  peers <- t( matrix(peers, 2) )

  # Inflate the peers
  # TODO Check rownames, only called from p_nca_cr_fdhi and p_nca_ce_fdhi
  #if (inflate) {
  #  N           <- length(loop.data$x)
  #  scope       <- loop.data$scope.theo
  #  inflate     <- sqrt( log(N, 10) / (8 * N) )
  #  inflate.x   <- inflate * abs(scope[2] - scope[1])
  #  inflate.y   <- inflate * abs(scope[4] - scope[3])
  #  peers[,1]   <- peers[,1] - ifelse(flip.x, -inflate.x, inflate.x)
  #  peers[,2]   <- peers[,2] + ifelse(flip.y, -inflate.y, inflate.y)
  #}

  # Remove points outside scope
  # TODO Check rownames, only called from p_nca_ce_fdhi
  #if (limit) {
  #  scope       <- loop.data$scope.theo
  #  peers[,1][ peers[,1] < min(scope[c(1,2)]) ] <- min(scope[c(1,2)])
  #  peers[,1][ peers[,1] > max(scope[c(1,2)]) ] <- max(scope[c(1,2)])
  #  peers[,2][ peers[,2] < min(scope[c(3,4)]) ] <- min(scope[c(3,4)])
  #  peers[,2][ peers[,2] > max(scope[c(3,4)]) ] <- max(scope[c(3,4)])
  #}

  rownames(peers) <- rownames
  colnames(peers) <- c("X", "Y")

  return (peers)
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

p_method_peers <- function (peers, plots, methods) {
  handled <- c("ce_vrs", "ce_fdh", "ols", "cols",
               "qr", "cr_vrs", "cr_fdh", "c_lp")
  diff <- setdiff(handled, ceilings)
  if (length(diff) != 0) {
    stop("Not all ceilings are handled in p_method_peers")
  }

  method_peers <- list()
  for (method in methods) {
    if (method == "ols") {
      next
    }
    if (method %in% p_ceilings_step) {
      method_peers[[method]] <- peers
    }
    if (method %in% p_ceilings_line) {
      tmp_list <- list()
      for (x.name in names(plots)) {
        plot <- plots[[x.name]]
        loop.data <- list(x=plot$x, y=plot$y,
                          flip.x=plot$flip.x, flip.y=plot$flip.y)
        df <- p_peers(loop.data, vrs = TRUE)

        if (method %in% c("c_lp", "qr", "cols")) {
          if (method %in% c("qr", "cols")) {
            intercept <- unname(coef(plot$lines[[method]])["(Intercept)"])
            scope <- unname(coef(plot$lines[[method]])["x"])
          }
          else {
            intercept <- plot$lines[[method]][1]
            scope <- plot$lines[[method]][2]
          }

          if (is.null(intercept) || is.null(scope)) {
            df <- NULL
          }
          else {
            df <- cbind(df, abs(intercept + scope * df[, 1] - df[, 2]))
            df <- df[order(df[, 3]),]
            delta <- abs(df[1, 2] / 1e4)
            df <- subset(df[, c(1, 2)], abs(df[, 3]) < delta)
          }
        }

        if (!is.null(df)) {
          tmp_list[[x.name]] <- df
        }
      }
      method_peers[[method]] <- tmp_list
    }
  }

  return( method_peers )
}

p_aggregate_peers <- function(model.peers, x)
{
  # Aggregate all ceiling peers for independent variable x
  peers <- NULL
  for (ceiling in names(model.peers)) {
    peers <- rbind(peers, model.peers[[ceiling]][[x]])
  }
  return ( unique(peers) )
}