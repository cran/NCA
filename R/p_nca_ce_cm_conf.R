p_nca_ce_cm_conf <-
function (loop.data, bn.data) {
  columns <- p_columns(loop.data, TRUE)
  line    <- p_conf_line(columns)
  peers   <- t(columns[c(2, 5), ])

  # Needed for p_con_lim and p_nca_cr_cm_conf
  attr(line, "columns") <- columns

  ceiling     <- p_ce_ceiling(loop.data, peers, "con")
  effect      <- ceiling / loop.data$scope.area
  ineffs      <- p_ineffs_ce(loop.data, peers)
  bottleneck  <- p_bottleneck_ce(loop.data, bn.data, peers, "fdh")
  fit         <- get_fit(ceiling, loop.data$ce_fdh_ceiling)

  return(list(line=line,
              slope=NA, intercept=NA,
              ceiling=ceiling, effect=effect,
              above=0, accuracy=100, fit=fit,
              ineffs=ineffs, bottleneck=bottleneck))
}
