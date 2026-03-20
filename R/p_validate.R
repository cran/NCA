p_validate_clean <-
  function (data, x, y, outliers = FALSE) {
    data <- as.data.frame(data)

    if (length(y) != 1) {
      message()
      stop("Dependent variable can only be a single column!\n\n", call. = F)
    }

    # Replace all non-numeric values with NA (will be discarded later on)
    for (i in seq_len(ncol(data))) {
      suppressWarnings(data[[i]] <- as.numeric(as.character(data[[i]])))
    }

    # Needed to collapse multiple scope warnings
    found <- FALSE
    for (i in 1:sys.nframe()) {
      haystack <- deparse(sys.calls()[[sys.nframe() - i + 1]])[1]
      found <- found || grepl("nca_outliers(", haystack, fixed = TRUE)
    }
    if (outliers == found) {
      .GlobalEnv$scope_warnings_nca <- NULL
    }

    return(list(x = data[x], y = data[y]))
  }

p_validate_ceilings <-
  function (methods) {
    ceilings <- c(ceilings, p_ceiling_custom)
    methods <- unique(methods)

    valid_methods <- intersect(tolower(methods), ceilings)
    if (length(valid_methods) == 0) {
      warning("Invalid ceilings, using ols, ce_fdh and cr_fdh", call. = FALSE)
      return(c("ols", "ce_fdh", "cr_fdh"))
    }

    if (length(valid_methods) != length(methods)) {
      for (diff in setdiff(methods, valid_methods)) {
        warning(sprintf("Ignoring invalid ceiling(s) '%s'", diff), call. = FALSE)
      }
    }
    return(valid_methods)
  }

p_validate_custom <-
  function (custom, data.x) {
    if (is.null(custom)) {
      return(NULL)
    }

    if (!(length(custom) %in% c(2, 2 * ncol(data.x)))) {
      extra <- ifelse(ncol(data.x) > 1, paste("or", 2 * ncol(data.x)), "")
      warning("Invalid custom, it should be of length 2 ", extra, call. = FALSE)
      return(NULL)
    }

    # We can not warn for 'outside scope': scope not yet defined

    if (length(custom) == 2) {
      custom <- rep(custom, ncol(data.x))
    }
    return(matrix(custom, ncol = 2, byrow = TRUE))
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
             "or a single Boolean!\n", call. = F)
      }
    }

    return(flip.x)
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
             "or a integer 1:4!\n", call. = F)
      }
    }

    # Check if all corners are 1=4
    if (!all(corner %in% 1:4)) {
      message()
      stop("All corners must be an integer between 1 and 4 !\n", call. = F)
    }

    # Check if corners are all upper or all lower
    if (!all(corner %in% c(1, 2)) && !all(corner %in% c(3, 4))) {
      message()
      stop("All corners need to be in the upper half (1 and 2) \n       ",
           "or the lower half (3 and 4).\n       ",
           "You can not mix upper and lower !\n", call. = F)
    }

    return(corner)
  }

p_validate_effect_aggregation <-
  function (effect_aggregation) {
    effect_aggregation <- intersect(c(2, 3, 4), effect_aggregation)
    if (length(effect_aggregation) > 0) {
      total <- paste(c(1, effect_aggregation), collapse = ', ')
      warning(paste("Using corners", total, "for effect_aggregation"), call. = FALSE)
      warning(paste("The effect aggregation argument will be deprecated:",
                    "For each condition only one corner can be analysed at a time.",
                    "For the analysis of multiple corners for a condition,",
                    "conduct multiple analyses."), call. = FALSE)
    }
    return(effect_aggregation)
  }
