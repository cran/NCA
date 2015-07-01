nca_cr_vrs <- 
function (loop.data, mpy, cutoff, bottleneck.x) {
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
  
  if (!is.vector(peers)) {
    # Perform OLS through the peers
    x <- peers[,3] + x.min
    y <- peers[,4] + y.min
    line <- lm(y~x)
    
    intercept <- unname(coef(line)["(Intercept)"])
    slope     <- unname(coef(line)["x"])
    ceiling   <- p_ceiling(loop.data, slope, intercept)
  } else {
    ceiling   <- 0
    intercept <- NA
    slope     <- NA
    line      <- NULL
  }
  
  effect      <- ceiling / loop.data$scope
  ineffs      <- p_ineffs(loop.data, intercept, slope, ceiling)
  above       <- p_above(loop.data, slope, intercept)
  bottleneck  <- p_bottleneck(loop.data, mpy, slope, intercept, cutoff, bottleneck.x)
  
  return(list(line=line,
              ceiling=ceiling, slope=slope, effect=effect,
              intercept=intercept, above=above, ineffs=ineffs,
              bottleneck=bottleneck))
}