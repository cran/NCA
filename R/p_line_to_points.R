# TODO Use functions everywhere

p_get_x <-
  function (intercept, slope, y.value) {
    return((y.value - intercept) / slope)
  }

p_get_y <-
  function (intercept, slope, x.value) {
    return(intercept + slope * x.value)
  }

p_x_on_scope <-
  function (x.value, scope) {
    return(x.value >= min(scope[1:2]) && x.value <= max(scope[1:2]))
  }

p_y_on_scope <-
  function (y.value, scope) {
    return(y.value >= min(scope[3:4]) && y.value <= max(scope[3:4]))
  }

p_through_scope <-
  function (intercept, slope, scope) {
    y.1 <- p_get_y(intercept, slope, scope[1])
    left_side <- p_y_on_scope(y.1, scope)
    y.2 <- p_get_y(intercept, slope, scope[2])
    right_side <- p_y_on_scope(y.2, scope)
    x.3 <- p_get_x(intercept, slope, scope[3])
    floor <- p_x_on_scope(x.3, scope)
    x.4 <- p_get_x(intercept, slope, scope[4])
    roof <- p_x_on_scope(x.4, scope)

    return(left_side || floor || roof || right_side)
  }

p_get_line_matrix <-
  function (intercept, slope, loop.data) {
    scope <- loop.data$scope.theo
    flip.x <- loop.data$flip.x
    flip.y <- loop.data$flip.y
    corner <- 1 + flip.x + 2 * flip.y

    y.1 <- p_get_y(intercept, slope, scope[1])
    left_side <- p_y_on_scope(y.1, scope)
    y.2 <- p_get_y(intercept, slope, scope[2])
    right_side <- p_y_on_scope(y.2, scope)
    x.3 <- p_get_x(intercept, slope, scope[3])
    floor <- p_x_on_scope(x.3, scope)
    x.4 <- p_get_x(intercept, slope, scope[4])
    roof <- p_x_on_scope(x.4, scope)

    line <- matrix(ncol = 2, nrow = 0)

    ############################################################################
    # Add start point
    ############################################################################
    if (corner == 1) {
      if (left_side) {
        line <- rbind(line, c(scope[1], scope[3]))
      }
      else if (slope < 0 && roof) {
        line <- rbind(line, c(x.4, scope[3]))
      }
    }
    else if (corner == 2) {
      if (slope > 0 && floor) {
        line <- rbind(line, c(scope[1], scope[3]))
      }
      else if (slope < 0 && roof) {
        line <- rbind(line, c(scope[1], scope[4]))
      }
    }
    else if (corner == 3) {
      if (left_side) {
        line <- rbind(line, c(scope[1], scope[4]))
      }
      if (slope > 0 && floor) {
        line <- rbind(line, c(x.3, scope[4]))
      }
    }
    else if (corner == 4) {
      if (slope > 0 && floor) {
        line <- rbind(line, c(scope[1], scope[3]))
      }
      else if (slope < 0 && roof) {
        line <- rbind(line, c(scope[1], scope[4]))
      }
    }

    ############################################################################
    # Add points on the scope
    ############################################################################
    if (left_side) {
      line <- rbind(line, c(scope[1], y.1))
    }
    if (right_side) {
      line <- rbind(line, c(scope[2], y.2))
    }
    if (floor) {
      line <- rbind(line, c(x.3, scope[3]))
    }
    if (roof) {
      line <- rbind(line, c(x.4, scope[4]))
    }

    ############################################################################
    # Add end point
    ############################################################################
    if (corner == 1) {
      if (slope > 0 && roof) {
        line <- rbind(line, c(scope[2], scope[4]))
      }
    }
    else if (corner == 2) {
      if (slope > 0) {
        if (roof) {
          line <- rbind(line, c(x.4, scope[3]))
        }
        else if (right_side) {
          line <- rbind(line, c(scope[2], scope[3]))
        }
      }
      else if (slope < 0 && right_side) {
        line <- rbind(line, c(scope[2], scope[3]))
      }
    }
    else if (corner == 3) {
      if (slope > 0 && roof) {
        line <- rbind(line, c(scope[2], scope[4]))
      }
      else if (slope < 0 && floor) {
        line <- rbind(line, c(scope[2], scope[3]))
      }
    }
    else if (corner == 4) {
      if (slope > 0 && right_side) {
        line <- rbind(line, c(scope[2], scope[4]))
      }
      else if (slope < 0) {
        if (floor) {
          line <- rbind(line, c(x.3, scope[4]))
        }
        else if (right_side) {
          line <- rbind(line, c(scope[2], scope[4]))
        }
      }
    }

    line <- line[order(line[, 1]),]

    return(line)
  }
