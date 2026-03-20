p_nca_cr_fdh <-
  function (loop.data, bn.data) {
    x <- loop.data$x
    y <- loop.data$y

    # Find the points on the ceiling ("PEERS")
    peers <- p_peers(loop.data)

    if (nrow(unique(peers)) > 1) {
      # Perform OLS through the peers
      x <- peers[, 1]
      y <- peers[, 2]
      line <- lm(y ~ x)

      intercept <- unname(coef(line)["(Intercept)"])
      slope <- unname(coef(line)["x"])
      line_matrix <- p_get_line_matrix(intercept, slope, loop.data)
      ceiling <- p_ceiling(loop.data, slope, intercept)
      above <- p_above(loop.data, slope, intercept)
    } else {
      line <- NULL
      line_matrix <- NULL
      ceiling <- 0
      intercept <- NA
      slope <- NA
      above <- 0
      peers <- matrix(, nrow = 0, ncol = 2)
    }

    effect <- ceiling / loop.data$scope.area
    accuracy <- p_accuracy(loop.data, above)
    accuracy_nof <- nrow(loop.data$x) - above
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
      metrics <- p_purity_metrics(df, condition, outcome, "cr_fdh")
    }

    return(list(line = line, line_matrix = line_matrix, peers = peers,
                slope = slope, intercept = intercept,
                ceiling = ceiling, effect = effect,
                above = above, accuracy = accuracy, accuracy_nof = accuracy_nof,
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
