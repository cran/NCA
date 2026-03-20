p_nca_qr <-
  function (loop.data, bn.data) {
    x <- loop.data$x
    y <- loop.data$y
    flip.y <- loop.data$flip.y
    # Make sure 0 <= qr.tau <= 1
    qr.tau <- loop.data$qr.tau
    if (qr.tau < 0 || qr.tau > 1) {
      qr.tau <- 0.95
    }

    if (!flip.y) {
      # Upper corners
      qr <- rq(y ~ x, tau = qr.tau)
    } else if (flip.y) {
      # Lower corner
      qr <- rq(y ~ x, tau = 1 - qr.tau)
    }

    intercept <- unname(coef(qr)["(Intercept)"])
    slope <- unname(coef(qr)["x"])
    line_matrix <- p_get_line_matrix(intercept, slope, loop.data)
    ceiling <- p_ceiling(loop.data, slope, intercept)
    effect <- ceiling / loop.data$scope.area
    above <- p_above(loop.data, slope, intercept)
    accuracy <- p_accuracy(loop.data, above)
    fit <- p_fit(ceiling, loop.data$ce_fdh_ceiling)
    ineffs <- p_ineffs(loop.data, slope, intercept)
    bottleneck <- p_bottleneck(loop.data, bn.data, slope, intercept)
    complexity <- 1
    metrics <- p_purity_empty

    return(list(line = qr, line_matrix = line_matrix, peers = NULL,
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
