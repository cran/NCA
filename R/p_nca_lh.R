p_nca_lh <-
function (loop.data, bn.data) {
  peers <- p_peers(loop.data)

  if (nrow(unique(peers)) > 1) {
    # Get coordinates for first and last peer
    x1 <- head(peers, n=1)[1]
    x2 <- tail(peers, n=1)[1]
    y1 <- head(peers, n=1)[2]
    y2 <- tail(peers, n=1)[2]

    slope     <- (y2 - y1) / (x2 - x1)
    intercept <- y2 - (slope * x2)
    line      <- c(intercept, slope)
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
    peers     <- matrix(, nrow = 0, ncol = 2)
  }

  accuracy    <- p_accuracy(loop.data, above)
  fit         <- get_fit(ceiling, loop.data$ce_fdh_ceiling)
  bottleneck  <- p_bottleneck(loop.data, bn.data, slope, intercept)

  return(list(line=line, peers=peers,
              slope=slope, intercept=intercept,
              ceiling=ceiling, effect=effect,
              above=above, accuracy=accuracy, fit=fit,
              ineffs=ineffs, bottleneck=bottleneck))
}
