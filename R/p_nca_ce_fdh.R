p_nca_ce_fdh <-
  function (loop.data, bn.data) {
    flip.x <- loop.data$flip.x
    flip.y <- loop.data$flip.y

    # Find the points on the ceiling ("PEERS")
    peers <- p_peers(loop.data)

    line <- p_fdh_line(loop.data$scope.theo, peers, flip.x, flip.y)
    line_matrix <- p_fdh_line(loop.data$scope.theo, peers, flip.x, flip.y)
    ceiling <- p_ce_ceiling(loop.data, peers, "fdh")
    effect <- ceiling / loop.data$scope.area
    ineffs <- p_ineffs_ce(loop.data, peers)
    bottleneck <- p_bottleneck_ce(loop.data, bn.data, peers, "fdh")
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
      metrics <- p_purity_metrics(df, condition, outcome, "ce_fdh")
    }

    return(list(line = line, line_matrix = line_matrix, peers = peers,
                slope = NA, intercept = NA,
                ceiling = ceiling, effect = effect,
                above = 0, accuracy = 100, fit = 100,
                ineffs = ineffs, bottleneck = bottleneck,
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

p_fdh_line <-
  function (scope, peers, flip.x, flip.y) {
    x.points <- NULL
    y.points <- NULL

    y.old <- scope[3 + flip.y]
    for (i in seq_len(nrow(peers))) {
      x.new <- peers[i, 1]
      y.new <- peers[i, 2]
      x.points <- c(x.points, x.new, x.new)
      y.points <- c(y.points, y.old, y.new)
      y.old <- y.new
    }

    x.points <- c(x.points, scope[2 - flip.x])
    y.points <- c(y.points, y.new)
    return(array(c(x.points, y.points), dim = c(length(x.points), 2)))
  }
