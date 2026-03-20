p_nca_custom <-
  function (loop.data, bn.data) {
    intercept <- loop.data$custom[loop.data$idx, 1]
    slope <- loop.data$custom[loop.data$idx, 2]

    line <- c(intercept, slope)
    line_matrix <- p_get_line_matrix(intercept, slope, loop.data)
    ceiling <- p_ceiling(loop.data, slope, intercept)
    above <- p_above(loop.data, slope, intercept)

    effect <- ceiling / loop.data$scope.area
    accuracy <- p_accuracy(loop.data, above)
    fit <- p_fit(ceiling, loop.data$ce_fdh_ceiling)
    ineffs <- p_ineffs(loop.data, slope, intercept)
    bottleneck <- p_bottleneck(loop.data, bn.data, slope, intercept)
    complexity <- 1
    metrics <- p_purity_empty

    if (!p_through_scope(intercept, slope, loop.data$scope.theo)) {
      name.x <- colnames(loop.data$x)
      name.y <- colnames(loop.data$y)
      warning("Custom line (", intercept, ", ", slope,
              ") is outside the scope of ", name.x, " - ", name.y, call. = FALSE)
    }

    return(list(line = line, line_matrix = line_matrix, peers = NULL,
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
