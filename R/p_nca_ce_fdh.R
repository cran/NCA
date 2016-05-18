p_nca_ce_fdh <-
function (loop.data, bn.data) {
  x <- loop.data$x
  y <- loop.data$y
  flip.x <- loop.data$flip.x
  flip.y <- loop.data$flip.y

  # Find the points on the ceiling ("PEERS")
  peers       <- p_peers(loop.data)

  line        <- p_fdh_line(loop.data, peers, flip.x, flip.y)
  ceiling     <- p_ce_ceiling (loop.data, peers, flip.x, flip.y, "fdh")
  effect      <- ceiling / loop.data$scope.area
  ineffs      <- p_ineffs_ce(loop.data, peers)
  bottleneck  <- p_bottleneck_ce(loop.data, bn.data, peers, "fdh")

  return(list(line=line,
              slope=NA, intercept=NA,
              ceiling=ceiling, effect=effect,
              ineffs=ineffs, above=0,
              bottleneck=bottleneck))
}

p_fdh_line <-
function (loop.data, peers, flip.x, flip.y) {
  x.points <- c(loop.data$scope.emp[1 + flip.x])
  y.points <- c(loop.data$scope.theo[3 + flip.y])

  y.old <- y.points[1]
  for (i in 1:nrow(peers)) {
    x.new <- peers[i, 1]
    y.new <- peers[i, 2]
    x.points <- c(x.points, x.new, x.new)
    y.points <- c(y.points, y.old, y.new)
    y.old <- y.new
  }

  x.points <- c(x.points, loop.data$scope.theo[2 - flip.x])
  y.points <- c(y.points, loop.data$scope.emp[4 - flip.y])
  return (list(x.points, y.points))
}