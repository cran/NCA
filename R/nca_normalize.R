nca_util_normalize <-
  function (data, scale = NULL, min_max = NULL) {
    scales <- p_scales(scale, ncol(data))
    min_max <- p_min_max(data, min_max)

    for (col_idx in seq_along(data)) {
      if (NA %in% scales[col_idx,]) {
        next
      }
      column <- data[[col_idx]]
      min_val <- min_max[col_idx, 1]
      max_val <- min_max[col_idx, 2]
      new_min <- scales[col_idx, 1]
      new_max <- scales[col_idx, 2]

      # Normalize according to min_max
      numerator <- column - min_val
      denominator <- (max_val - min_val)
      org_norm <- numerator / denominator

      # Scale according to scale
      scale <- (new_max - new_min)
      column <- org_norm * scale + new_min

      # avoid floating-point error
      tol <- sqrt(.Machine$double.eps) * abs(new_max - new_min)
      column[abs(column - new_min) < tol] <- new_min
      column[abs(column - new_max) < tol] <- new_max
      column <- pmax(new_min, pmin(new_max, column))
      data[, col_idx] <- column
    }
    return(data)
  }

p_scales <-
  function (scales, no.cols) {
    if (is.null(scales)) {
      scales <- c(0, 1)
    }
    if (length(scales) == 2) {
      return(t(replicate(no.cols, scales)))
    }

    req_length <- 2 * no.cols
    if (length(scales) == req_length) {
      return(t(matrix(scales, nrow = 2)))
    }

    stop("The length of scale need to be 2 or ", req_length)
  }

p_min_max <-
  function (data, min_max) {
    data_min_max <- matrix(nrow = ncol(data), ncol = 2)
    for (col_idx in seq_along(data)) {
      data_min_max[col_idx,] <- c(min(data[[col_idx]], na.rm = TRUE),
                                  max(data[[col_idx]], na.rm = TRUE))
    }

    if (is.null(min_max)) {
      return(data_min_max)
    }

    no.cols <- ncol(data)
    if (length(min_max) == 2) {
      # One mix-max for all columns (not ideal)
      min_max <- rep(min_max, no.cols)
    }
    else if (length(min_max) != (2 * no.cols)) {
      stop("The length of min_max need to be 2 or ", 2 * no.cols)
    }
    min_max <- t(matrix(min_max, nrow = 2))

    for (idx in seq_along(data)) {
      if (min_max[idx, 1] > data_min_max[idx, 1] || min_max[idx, 2] < data_min_max[idx, 2]) {
        msg <- sprintf("Extending theoretical scope for '%s' from c(%s) to c(%s)",
                       colnames(data)[idx],
                       paste(min_max[idx,], collapse = ', '),
                       paste(data_min_max[idx,], collapse = ', '))
        warning(msg, call. = FALSE)
      }
      min_max[idx, 1] <- min(data_min_max[idx, 1], min_max[idx, 1])
      min_max[idx, 2] <- max(data_min_max[idx, 2], min_max[idx, 2])
    }

    return(min_max)
  }
