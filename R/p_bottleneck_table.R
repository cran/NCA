p_bottleneck_data <-
function (x, y, scope, flip.y, ceilings, bottleneck.x, bottleneck.y, steps, step.size, cutoff) {
  bn.x <- p_validate_bottleneck(bottleneck.x, "x")
  bn.y <- p_validate_bottleneck(bottleneck.y, "y")
  bn.x.id <- p_bottleneck_id(bn.x)
  bn.y.id <- p_bottleneck_id(bn.y)

  bottleneck.xy <- p_mp_mpy(y, scope, steps, step.size, bn.y.id, flip.y)
  mp <- as.data.frame(bottleneck.xy[[1]])
  mpy <- bottleneck.xy[[2]]

  attr(mp, "bn.x") <- bn.x
  attr(mp, "bn.y") <- bn.y
  attr(mp, "bn.y.id") <- bn.y.id
  attr(mp, "size") <- nrow(x)
  attr(mp, "cutoff") <- cutoff

  bottlenecks <- list()
  for (ceil in setdiff(ceilings, p_no_bottleneck)) {
    bottlenecks[[ceil]] <- mp
  }

  return( list(
    bottlenecks=bottlenecks,
    bn.x=bn.x,
    bn.y=bn.y,
    bn.x.id=bn.x.id,
    bn.y.id=bn.y.id,
    mpy=mpy,
    cutoff=cutoff,
    steps=steps
  ) )
}

p_mp_mpy <-
function (y, scope, steps, step.size, bn.y.id, flip.y) {
  if (length(steps) == 1 && steps < 1) {
    steps <- 10
  }

  if (bn.y.id == 3) {
    mp_mpy <- p_mp_mpy_actual(y, scope, steps, step.size, flip.y)
  }
  else {
    mp_mpy <- p_mp_mpy_perc(y, scope, steps, step.size, bn.y.id, flip.y)
  }

  colnames(mp_mpy[[1]]) <- colnames(y)
  colnames(mp_mpy[[2]]) <- colnames(y)

  return( mp_mpy )
}

p_low_high <-
function (y, scope, bn.y.id) {
  # Try to get the low/high values from the scope
  py.low <- scope[[1]][3]
  py.high <- scope[[1]][4]

  # Scope might be NULL
  if (is.null(py.low) || is.null(py.high)) {
    py.low <- min(y, na.rm=TRUE)
    py.high <- max(y, na.rm=TRUE)
  } else {
    py.low <- min(y, py.low, na.rm=TRUE)
    py.high <- max(y, py.high, na.rm=TRUE)
  }

  # User want from zero
  if (bn.y.id == 2) {
    py.low <- 0
  }

  return ( c(py.low, py.high) )
}

p_sanitize_steps <-
function (steps, low, high) {
  steps <- sort(steps)

  if (low > steps[1]) {
    message("\nSome steps below scope, excluded")
    steps <- steps[steps >= low]
  }

  if (high < steps[length(steps)]) {
    message("\nSome steps above scope, excluded")
    steps <- steps[steps <= high]
  }

  return (steps)
}

p_mp_mpy_actual <-
function (y, scope, steps, step.size, flip.y) {
  py.low.high <- p_low_high(y, scope, 3)
  py.low <- py.low.high[1]
  py.high <- py.low.high[2]

  if (is.null(step.size) || step.size <= 0) {
    if (length(steps) > 1) {
      # Interpret the list of steps as the values
      values <- p_sanitize_steps(steps, py.low, py.high)
    }
    else {
      # Single step
      step <- (py.high - py.low) / steps
      values <- seq(py.low, py.high, by=step)
    }
  } else {
    values <- NULL
    value <- py.low
    while (value <= py.high) {
      values <- c(values, value)
      value <- value + step.size
    }
    if (abs(values[length(values)] - py.high) > 1E-6) {
      values <- c(values, py.high)
    }
  }

  if (flip.y) {
    mpy <- matrix(rev(values), ncol=1)
  } else {
    mpy <- matrix(values, ncol=1)
  }

  return (list(mpy, mpy))
}

p_mp_mpy_perc <-
function (y, scope, steps, step.size, bn.y.id, flip.y) {
  py.low.high <- p_low_high(y, scope, bn.y.id)
  py.low <- py.low.high[1]
  py.high <- py.low.high[2]

  if (is.null(step.size) || step.size <= 0) {
    if (length(steps) > 1) {
      # Interpret the list of steps as the probabilities (0 - 100)
      probs <- p_sanitize_steps(steps, 0, 100) / 100
    }
    else {
      # Single step
      probs <- seq(0, 1, length.out=steps + 1)
    }
  } else {
    probs <- seq(0, 1, by=min(1000, step.size / 100))
  }

  if (bn.y.id == 4) {
    values <- quantile(y[[1]], probs, na.rm=TRUE)
  }
  else {
    values <- py.low + probs * (py.high - py.low)
  }

  if (flip.y) {
    mpy <- matrix(rev(values), ncol=1)
  } else {
    mpy <- matrix(values, ncol=1)
  }
  mp <- matrix(100 * probs, ncol=1)

  return (list(mp, mpy))
}

p_bottleneck_options <- list(
  "percentage.range",
  "percentage.max",
  "actual",
  "percentile"
)

p_validate_bottleneck <-
function (option, x_or_y) {
  if (is.na(match(option, p_bottleneck_options))) {
    message(paste0("\nBottleneck option '", option, "' for ", x_or_y,
      " is not valid, using '", p_bottleneck_options[[1]], "'"))
    return(p_bottleneck_options[[1]])
  }
  return(option)
}

p_bottleneck_id <-
function (option) {
  if (is.na(match(option, p_bottleneck_options))) {
    return(1)
  }
  return(match(option, p_bottleneck_options))
}
