nca_random <- function (n, intercepts, slopes, corner = 1,
                        distribution.x = "uniform", distribution.y = "uniform",
                        mean.x = 0.5, mean.y = 0.5, sd.x = 0.2, sd.y = 0.2) {
  # Validate the inputs
  tmp <- p_validate_inputs(n, intercepts, slopes, corner, distribution.x, distribution.y)
  error <- tmp$error
  intercepts <- tmp$intercepts
  slopes <- tmp$slopes

  if (!is.null(error)) {
    stop(p_errors[error])
  }

  data <- NULL
  if (length(slopes) == 1) {
    data[['X']] <- NA
  }
  else {
    for (i in 1:length(slopes)) {
      data[[paste0('X', i)]] <- NA
    }
  }
  data[['Y']] <- NA
  data <- data.frame(data)
  for (idx in 1:n) {
    while (TRUE) {
      # Generate random point within [0, 1], check if they conform to the line
      y.value <- p_value(distribution.y, mean.y, sd.y)
      data[idx, length(slopes) + 1] <- y.value

      all.good <- TRUE
      for (x.idx in 1:length(slopes)) {
        x.value <- p_value(distribution.x, mean.x, sd.x)
        data[idx, x.idx] <- x.value

        slope <- slopes[x.idx]
        intercept <- intercepts[x.idx]
        if (corner %in% c(1, 2) && y.value >= min(1, (intercept + slope * x.value))) {
          all.good <- FALSE
        }
        if (corner %in% c(3, 4) && y.value <= max(0, (intercept + slope * x.value))) {
          all.good <- FALSE
        }
      }
      if (all.good) {
        break
      }
    }
  }
  data[order(data$Y),]
  rownames(data) <- 1:n

  attr(data, "distribution.x") <- distribution.x
  attr(data, "distribution.y") <- distribution.y
  if (distribution.x == "normal") {
    attr(data, "mean.x") <- mean.x
    attr(data, "sd.x") <- sd.x
  }
  if (distribution.y == "normal") {
    attr(data, "mean.y") <- mean.y
    attr(data, "sd.y") <- sd.y
  }

  return(data)
}

p_value <- function (distribution, mean, sd)
{
  if (distribution == "uniform") {
    return(runif(1, 0, 1))
  }
  else {
    return(rtruncnorm(1, a = 0, b = 1, mean = mean, sd = sd))
  }
}

p_errors <- list(
  n = "n should be an integer > 1!",
  combination = "The combination of slope and intercept does not provide points in the [(0, 0), (1, 1)] area!",
  length = "The length of the slopes and intercepts should be equal!",
  corner_23 = "Upward slope can not provide empty corners 2 (upper right) or 3 (bottom left)!",
  corner_14 = "Downward slope can not provide empty corners 1 (upper left) or 4 (bottom right)!",
  distribution = "The distribution types need to be 'uniform' or 'normal'!"
)

p_validate_inputs <- function (n, intercepts, slopes, corner, distribution.x, distribution.y)
{
  if (n < 1 || n != round(n)) {
    return(list(error = 'n', intercepts = intercepts, slopes = slopes))
  }

  if (length(intercepts) == 1 && length(slopes) > 1)
  {
    intercepts <- rep(intercepts, length(slopes))
  }
  else if (length(intercepts) > 1 && length(slopes) == 1) {
    slopes <- rep(slopes, length(intercepts))
  }

  if (length(intercepts) != length(slopes)) {
    return(list(error = 'length', intercepts = intercepts, slopes = slopes))
  }

  for (idx in 1:length(slopes)) {
    slope <- slopes[idx]
    intercept <- intercepts[idx]
    cond.1 <- slope > 0 && (intercept >= 1 || (intercept + slope) <= 0)
    cond.2 <- slope < 0 && (intercept <= 0 || (intercept + slope) >= 1)
    cond.3 <- slope == 0 && (intercept <= 0 || intercept >= 1)
    if (cond.1 || cond.2 || cond.3) {
      return(list(error = 'combination', intercepts = intercepts, slopes = slopes))
    }

    if (slope > 0 && (corner %in% c(2, 3))) {
      return(list(error = 'corner_23', intercepts = intercepts, slopes = slopes))
    }
    if (slope < 0 && (corner %in% c(1, 4))) {
      return(list(error = 'corner_14', intercepts = intercepts, slopes = slopes))
    }
  }

  if (length(setdiff(c(distribution.x, distribution.y), c("uniform", "normal"))) != 0) {
    return(list(error = 'distribution', intercepts = intercepts, slopes = slopes))
  }

  return(list(error = NULL, intercepts = intercepts, slopes = slopes))
}
