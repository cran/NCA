p_nca_c_lp <-
function (loop.data, bn.data) {
  peers <- p_peers(loop.data)

  if (!is.vector(peers) && length(peers) > 2) {
    K <- length(peers[,1])
    factor = ifelse(sum(loop.data$flip.x, loop.data$flip.y) == 1, -1, 1)
    f.obj <- c(K, -K, factor * sum(peers[,1]))
    f.con <- cbind(rep(1, K), rep(-1, K), factor * peers[,1])
    f.dir <- rep(ifelse(loop.data$flip.y, "<=", ">="), K)
    f.rhs <- peers[, 2]
    sol   <- lp(ifelse(loop.data$flip.y, "max", "min"),
                f.obj, f.con, f.dir, f.rhs)$solution
    # Intercept can always be negative
    intercept <- sol[[1]] - sol[[2]]
    # Slope needs to be positive in corner 1 and 4
    slope     <- factor * sol[[3]]
    line      <- c(intercept, slope)
    ceiling   <- p_ceiling(loop.data, slope, intercept)
    above     <- 0
  } else {
    line      <- NULL
    slope     <- NA
    intercept <- NA
    ceiling   <- 0
    above     <- NA
  }

  effect      <- ceiling / loop.data$scope.area
  accuracy    <- p_accuracy(loop.data, above)
  fit         <- get_fit(ceiling, loop.data$ce_fdh_ceiling)
  ineffs      <- p_ineffs(loop.data, slope, intercept)
  bottleneck  <- p_bottleneck(loop.data, bn.data, slope, intercept)

  return(list(line=line,
              slope=slope, intercept=intercept,
              ceiling=ceiling, effect=effect,
              above=above, accuracy=accuracy, fit=fit,
              ineffs=ineffs, bottleneck=bottleneck))
}
