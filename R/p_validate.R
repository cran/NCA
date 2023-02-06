p_validate_clean <-
function (data, x, y) {
  data <- as.data.frame(data)

  if (length(y) != 1) {
    message()
    stop("Dependent variable can only be a single column!\n\n", call.=F)
  }

  # Replace all non-numeric values with NA (will be discarded later on)
  for (i in 1:ncol(data)) {
    suppressWarnings(data[[i]] <- as.numeric(as.character(data[[i]])))
  }

  return ( list(x=data[x], y=data[y]) )
}

p_validate_ceilings <-
function (methods) {
  valid_methods <- intersect(tolower(methods), ceilings)

  if (length(valid_methods) == 0) {
    warning("Invalid ceilings, using ols, ce_fdh and cr_fdh", call.=FALSE)
    return ( c("ols", "ce_fdh", "cr_fdh") )
  } else if (length(valid_methods) != length(methods)) {
    for (diff in setdiff(methods, valid_methods)) {
      warning(sprintf("Ignoring invalid ceiling(s) '%s'", diff), call.=FALSE)
    }
  }

  return ( valid_methods )
}

p_validate_flipx <-
function (x, flip.x) {
  if (length(flip.x) != length(x)) {
    # Use single value for all Xs
    if (length(flip.x) == 1) {
      # Use single value for all Xs
      flip.x <- replicate(length(x), flip.x)
    } else {
      message()
      stop("The length of 'flip.x' needs to be equal to the length of x ",
           "or a single Boolean!\n", call.=F)
    }
  }

  return ( flip.x )
}

p_validate_corner <-
function (x, corner) {
  # Check length of (or create) the vector
  if (length(corner) != length(x)) {
    # Use single value for all Xs
    if (length(corner) == 1) {
      # Use single value for all Xs
      corner <- replicate(length(x), corner)
    } else {
      message()
      stop("The length of 'corner' needs to be equal to the length of x ",
           "or a integer 1:4!\n", call.=F)
    }
  }

  # Check if all corners are 1=4
  if (!all(corner %in% 1:4)) {
    message()
    stop("All corners must be an integer between 1 and 4 !\n", call.=F)
  }

  # Check if corners are all on top or all on bottom
  if (!all(corner %in% c(1, 2)) && !all(corner %in% c(3, 4))) {
    message()
    stop("All corners need to be in the top half (1 and 2) \n       ",
         "or the bottom half (3 and 4).\n       ",
         "You can not mix top and bottom !\n", call.=F)
  }

  return (corner)
}
