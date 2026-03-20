p_above <-
  function (loop.data, slope, intercept) {
    x <- loop.data$x
    y <- loop.data$y
    flip.x <- loop.data$flip.x
    flip.y <- loop.data$flip.y

    # Upper left and lower right
    if ((flip.x == flip.y) && (is.na(slope) || slope < 0)) {
      return(NaN)
    }
    # Lower left and upper right
    if ((flip.x != flip.y) && (is.na(slope) || slope > 0)) {
      return(NaN)
    }

    # Vertical difference between observations and ceiling line
    y.c <- slope * x + intercept
    if (!flip.y) {
      y.diff <- y - y.c
    } else {
      y.diff <- y.c - y
    }

    return(sum(y.diff > 1e-07, na.rm = TRUE))
  }

p_accuracy <-
  function (loop.data, above) {
    nObservations <- min(length(loop.data$x), length(loop.data$y))
    return(100 * (nObservations - above) / nObservations)
  }

p_fit <-
  function (ceiling, fdh_ceiling) {
    if (is.nan(ceiling) || is.nan(fdh_ceiling)) {
      return(NA)
    }
    if (ceiling > fdh_ceiling) {
      return(NA)
    }
    return(100 - 100 * abs(ceiling - fdh_ceiling) / fdh_ceiling)
  }
