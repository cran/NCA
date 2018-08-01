p_nca_ce_fdh <-
function (loop.data, bn.data) {
  flip.x <- loop.data$flip.x
  flip.y <- loop.data$flip.y

  # Find the points on the ceiling ("PEERS")
  peers       <- p_peers(loop.data)

  line        <- p_fdh_line(loop.data$scope.theo, peers, flip.x, flip.y)
  ceiling     <- p_ce_ceiling(loop.data, peers, "fdh")
  effect      <- ceiling / loop.data$scope.area
  ineffs      <- p_ineffs_ce(loop.data, peers)
  bottleneck  <- p_bottleneck_ce(loop.data, bn.data, peers, "fdh")

  return(list(line=line,
              slope=NA, intercept=NA,
              ceiling=ceiling, effect=effect,
              above=0, accuracy=100, fit=100,
              ineffs=ineffs, bottleneck=bottleneck,
              peers=peers))
}

p_fdh_line <-
function (scope, peers, flip.x, flip.y) {
  x.points <- c()
  y.points <- c()

  y.old <- scope[3 + flip.y]
  for (i in 1:nrow(peers)) {
    x.new <- peers[i, 1]
    y.new <- peers[i, 2]
    x.points <- c(x.points, x.new, x.new)
    y.points <- c(y.points, y.old, y.new)
    y.old <- y.new
  }

  x.points <- c(x.points, scope[2 - flip.x])
  y.points <- c(y.points, y.new)
  return (list(x.points, y.points))
}