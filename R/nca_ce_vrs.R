nca_ce_vrs <-
function (loop.data, ...) {
  x <- loop.data$x
  y <- loop.data$y  
  
  # Transform the X and Y axes into positive values (for DEA)
  x.min <- min(x, 0)
  y.min <- min(y, 0)
  x <- x - x.min
  y <- y - y.min  
  
  # Find the points on the ceiling ("PEERS")
  # with optimal technical efficiency for DEA (vrs)
  vrs <- dea(x, y, RTS="vrs", ORIENTATION="graph")
  
  # Get the sorted, corrected peer matrix
  peers <- p_optimal_peers(vrs, x, y)

  # Transform the X and Y back
  peers[, 3] <- peers[, 3] + x.min
  peers[, 4] <- peers[, 4] + y.min
  
  # if there is only one peer, the ceiling zone is zero
  ceiling <- 0
  if (!is.vector(peers)) {
    for (i in 1:(nrow(peers)-1)) {
      part.a <- (peers[i+1,4] - peers[i,4]) * (peers[i+1,3] - peers[1,3])
      part.b <- 0.5 * (peers[i+1,4] - peers[i,4]) * (peers[i+1,3] - peers[i,3])
      ceiling <- ceiling + part.a - part.b
    }
  }
  effect <- ceiling / loop.data$scope
  ineffs <- p_ineffs_ce(loop.data, peers, ceiling)
  
  return(list(line=list(x + x.min, y + y.min),
              ceiling=ceiling, slope=NA, effect=effect,
              intercept=NA, above=0, ineffs=ineffs,
              bottleneck=NULL))
}