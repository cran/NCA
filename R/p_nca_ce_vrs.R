p_nca_ce_vrs <-
  function (loop.data, bn.data) {
    flip.x <- loop.data$flip.x
    flip.y <- loop.data$flip.y

    # Find the points on the ceiling ("PEERS")
    peers <- p_peers(loop.data, vrs = TRUE)

    line <- p_vrs_line(loop.data, peers, flip.x, flip.y)
    line_matrix <- p_vrs_line(loop.data, peers, flip.x, flip.y)
    ceiling <- p_ce_ceiling(loop.data, peers, "vrs")
    effect <- ceiling / loop.data$scope.area
    fit <- p_fit(ceiling, loop.data$ce_fdh_ceiling)
    ineffs <- p_ineffs_ce(loop.data, peers)
    bottleneck <- p_bottleneck_ce(loop.data, bn.data, peers, "vrs")
    complexity <- nrow(unique(peers))

    metrics <- p_purity_empty
    # Extra metrics for certain lines
    skip <- !is.null(loop.data[[p_skip_purity]]) || Sys.getenv(p_skip_purity) == TRUE
    skip <- skip || loop.data$flip.x || loop.data$flip.y
    if (!skip) {
      df <- data.frame(loop.data$x, loop.data$y)
      condition <- colnames(loop.data$x)
      outcome <- colnames(loop.data$y)
      colnames(df) <- c(condition, outcome)
      metrics <- p_purity_metrics(df, condition, outcome, "ce_vrs")
    }

    return(list(line = line, line_matrix = line_matrix, peers = peers,
                slope = NA, intercept = NA,
                ceiling = ceiling, effect = effect,
                above = 0, accuracy = 100, accuracy_nof = nrow(loop.data$x),
                fit = fit, ineffs = ineffs, bottleneck = bottleneck,
                complexity = complexity,
                noise_pct = metrics$noise_pct,
                noise_nof = metrics$noise_nof,
                exceptions_pct = metrics$exceptions_pct,
                exceptions_nof = metrics$exceptions_nof,
                support_pct = metrics$support_pct,
                support_nof = metrics$support_nof,
                spread = metrics$spread,
                sharpness = metrics$sharpness
    ))
  }

p_vrs_line <-
  function (loop.data, peers, flip.x, flip.y) {
    x.points <- c(loop.data$scope.emp[1 + flip.x],
                  peers[, 1],
                  loop.data$scope.theo[2 - flip.x])
    y.points <- c(loop.data$scope.theo[3 + flip.y],
                  peers[, 2],
                  loop.data$scope.emp[4 - flip.y])
    return(array(c(x.points, y.points), dim = c(length(x.points), 2)))
  }
