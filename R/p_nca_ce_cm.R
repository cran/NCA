p_nca_ce_cm <-
function (loop.data, bn.data) {
  columns     <- p_columns(loop.data, FALSE)
  cm_peers    <- t(columns[c(2, 5), ])

  line        <- p_conf_line(columns)
  ceiling     <- p_cm_ceiling(loop.data, cm_peers)
  effect      <- ceiling / loop.data$scope.area
  ineffs      <- p_ineffs_ce(loop.data, cm_peers)
  fit         <- get_fit(ceiling, loop.data$ce_fdh_ceiling)

  return(list(line=line,
              slope=NA, intercept=NA,
              ceiling=ceiling, effect=effect,
              above=0, accuracy=100, fit=fit,
              ineffs=ineffs, bottleneck=NULL))
}

