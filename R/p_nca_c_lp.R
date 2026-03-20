p_nca_c_lp <-
  function (loop.data, bn.data) {
    peers <- p_peers(loop.data)

    if (nrow(unique(peers)) > 1) {
      K <- nrow(peers)
      factor <- ifelse(sum(loop.data$flip.x, loop.data$flip.y) == 1, -1, 1)
      f.obj <- c(K, -K, factor * sum(peers[, 1]))
      f.con <- cbind(rep(1, K), rep(-1, K), factor * peers[, 1])
      f.dir <- rep(ifelse(loop.data$flip.y, "<=", ">="), K)
      f.rhs <- peers[, 2]
      sol <- lp(ifelse(loop.data$flip.y, "max", "min"),
                f.obj, f.con, f.dir, f.rhs)$solution

      # Intercept can always be negative
      intercept <- sol[[1]] - sol[[2]]
      # Slope needs to be positive in corner 1 and 4
      slope <- factor * sol[[3]]
      line <- c(intercept, slope)
      line_matrix <- p_get_line_matrix(intercept, slope, loop.data)
      ceiling <- p_ceiling(loop.data, slope, intercept)
      above <- 0
      # Find the peers on the line
      peers <- p_best_peers(peers, intercept, slope)
    } else {
      line <- NULL
      line_matrix <- NULL
      slope <- NA
      intercept <- NA
      ceiling <- 0
      above <- NA
      peers <- matrix(, nrow = 0, ncol = 2)
    }

    effect <- ceiling / loop.data$scope.area
    accuracy <- p_accuracy(loop.data, above)
    fit <- p_fit(ceiling, loop.data$ce_fdh_ceiling)
    ineffs <- p_ineffs(loop.data, slope, intercept)
    bottleneck <- p_bottleneck(loop.data, bn.data, slope, intercept)
    complexity <- 1

    metrics <- p_purity_empty
    # Extra metrics for certain lines
    skip <- !is.null(loop.data[[p_skip_purity]]) || Sys.getenv(p_skip_purity) == TRUE
    skip <- skip || loop.data$flip.x || loop.data$flip.y
    if (!skip) {
      df <- data.frame(loop.data$x, loop.data$y)
      condition <- colnames(loop.data$x)
      outcome <- colnames(loop.data$y)
      colnames(df) <- c(condition, outcome)
      metrics <- p_purity_metrics(df, condition, outcome, "c_lp")
    }

    return(list(line = line, line_matrix = line_matrix, peers = peers,
                slope = slope, intercept = intercept,
                ceiling = ceiling, effect = effect,
                above = above, accuracy = accuracy, fit = fit,
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
