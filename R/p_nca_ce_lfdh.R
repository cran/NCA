p_nca_ce_lfdh <-
function (loop.data, bn.data) {
  flip.x <- loop.data$flip.x
  flip.y <- loop.data$flip.y

  # Find the points on the ceiling ("PEERS")
  peers       <- p_peers(loop.data)

  line        <- p_vrs_line(loop.data, peers, flip.x, flip.y)
  ceiling     <- p_ce_ceiling(loop.data, peers, "vrs")
  effect      <- ceiling / loop.data$scope.area
  ineffs      <- p_ineffs_ce(loop.data, peers)
  bottleneck  <- p_bottleneck_ce(loop.data, bn.data, peers, "vrs")
  fit         <- get_fit(ceiling, loop.data$ce_fdh_ceiling)

  return(list(line=line,
              slope=NA, intercept=NA,
              ceiling=ceiling, effect=effect,
              above=0, accuracy=100, fit=fit,
              ineffs=ineffs, bottleneck=bottleneck))
}
