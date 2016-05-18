p_validate_clean <-
function (data, x, y) {
  data <- as.data.frame(data)

  if (length(y) != 1) {
    message()
    stop("Dependent variable can only be a single column!\n\n", call. = FALSE)
  }

  # Replace all non-numeric values with NA (will be discarded later on)
  for (i in 1:ncol(data)) {
    suppressWarnings(data[[i]] <- as.numeric(as.character(data[[i]])))
  }

  return ( list(x=data[x], y=data[y]) )
}

p_validate_ceilings <-
function (methods) {
  if (is.null(methods) || length(methods) == 0) {
    warning("Invalid ceilings, using ols, ce_fdh and cr_fdh\n\n", call.=FALSE)
    return ( c("ols", "ce_fdh", "cr_fdh") )
  }

  valid_methods <- intersect(methods, ceilings)

  if (length(valid_methods) == 0) {
    message()
    stop("\nAt least 1 valid ceiling technique needs to be defined!\n\n", call.=FALSE)
  } else if (length(valid_methods) != length(methods)) {
    message()
    warning(sprintf("Ignoring invalid ceiling(s) %s\n\n", setdiff(methods, valid_methods)), call.=FALSE)
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
      stop("The length of flip.x needs to be equal to the length of x ",
           "or a single Boolean!\n", call. = FALSE)
    }
  }

  return ( flip.x )
}