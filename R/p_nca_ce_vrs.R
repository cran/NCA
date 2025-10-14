p_nca_ce_vrs <-
function (loop.data, bn.data) {
  flip.x <- loop.data$flip.x
  flip.y <- loop.data$flip.y
  
  # Find the points on the ceiling ("PEERS")
  peers       <- p_peers(loop.data, vrs=TRUE)

  line        <- p_vrs_line(loop.data, peers, flip.x, flip.y)
  ceiling     <- p_ce_ceiling(loop.data, peers, "vrs")
  effect      <- ceiling / loop.data$scope.area
  ineffs      <- p_ineffs_ce(loop.data, peers)
  bottleneck  <- p_bottleneck_ce(loop.data, bn.data, peers, "vrs")
  fit         <- get_fit(ceiling, loop.data$ce_fdh_ceiling)

  return(list(line=line, peers=peers,
              slope=NA, intercept=NA,
              ceiling=ceiling, effect=effect,
              above=0, accuracy=100, fit=fit,
              ineffs=ineffs, bottleneck=bottleneck))
}

p_vrs_line <-
function (loop.data, peers, flip.x, flip.y) {
  x.points <- c(loop.data$scope.emp[1 + flip.x],
                peers[,1],
                loop.data$scope.theo[2 - flip.x])
  y.points <- c(loop.data$scope.theo[3 + flip.y],
                peers[,2],
                loop.data$scope.emp[4 - flip.y])
  return ( list(x.points, y.points) )
}
