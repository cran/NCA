nca_ce_fdh <-
function (loop.data, mpy, cutoff, bottleneck.x) {
  x <- loop.data$x
  y <- loop.data$y 

  # Transform the X and Y axes into positive values (for DEA)
  x.min <- min(x, 0)
  y.min <- min(y, 0)
  x <- x - x.min
  y <- y - y.min  
  
  # Find the points on the ceiling ("PEERS")
  # with optimal technical efficiency for DEA(fdh)
  fdh <- dea(x, y, RTS="fdh", ORIENTATION="graph")

  # Get the sorted, corrected peer matrix
  peers <- p_optimal_peers(fdh, x, y)

  # Transform the X and Y back
  peers[, 3] <- peers[, 3] + x.min
  peers[, 4] <- peers[, 4] + y.min

  # if there is only one peer, the ceiling zone is zero
  ceiling <- 0
  if (!is.vector(peers)) {
    for (i in 1:(nrow(peers)-1)) {
      ceiling <- ceiling + (peers[i+1,3] - loop.data$x.low) * (peers[i+1,4] - peers[i,4])
    }
  }
  
  effect      <- ceiling / loop.data$scope
  ineffs      <- p_ineffs_ce(loop.data, peers, ceiling)
  bottleneck  <- p_bottleneck_fdh(loop.data, mpy, peers, cutoff, bottleneck.x)
  
  return(list(line=list(x + x.min, y + y.min),
              ceiling=ceiling, slope=NA, effect=effect,
              intercept=NA, above=0, ineffs=ineffs,
              bottleneck=bottleneck))
}