p_nca_c_lp <-
function (loop.data, bn.data) {
  peers <- p_peers(loop.data)

  if (!is.vector(peers) && length(peers) > 2) {
    K <- length(peers[,1])
    A <- c(K, sum(peers[,1]))
    B <- cbind(rep(1, K), peers[,1])
    C <- rep(">=", K)

    line      <- lp("min", A, B, C, peers[,2])$solution
    slope     <- line[[2]]
    intercept <- line[[1]]
    ceiling   <- p_ceiling(loop.data, slope, intercept)
    effect    <- ceiling / loop.data$scope.area
    ineffs    <- p_ineffs(loop.data, slope, intercept)
    above     <- 0
    accuracy  <- 100
    fit       <- get_fit(ceiling, loop.data$ce_fdh_ceiling)
  } else {
    line      <- NULL
    slope     <- NA
    intercept <- NA
    ceiling   <- 0
    effect    <- 0
    ineffs    <- list(x=NA, y=NA, abs=NA, rel=NA)
    above     <- NA
    accuracy  <- NA
  }

  fit         <- get_fit(ceiling, loop.data$ce_fdh_ceiling)
  bottleneck  <- p_bottleneck(loop.data, bn.data, slope, intercept)

  return(list(line=line,
              slope=slope, intercept=intercept,
              ceiling=ceiling, effect=effect,
              above=above, accuracy=accuracy, fit=fit,
              ineffs=ineffs, bottleneck=bottleneck))
}
