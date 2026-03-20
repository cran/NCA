p_generate_title <-
  function (x.name, y.name) {
    return(paste(x.name, y.name, sep = " - "))
  }

p_pretty_name <-
  function (uglyName) {
    return(gsub("_", "-", toupper(uglyName)))
  }

p_is_number <-
  function (number) {
    if (typeof(number) == "list") {
      un_list = unlist(number, use.names = FALSE)
      if (is.null(un_list) || is.infinite(un_list)) {
        return(FALSE)
      }
    }

    if (is.null(number) ||
      is.na(number) ||
      number == "NA" ||
      number == "NN") {
      return(FALSE)
    } else {
      return(TRUE)
    }
  }

p_pretty_number <-
  function (uglyNumber, default = "", prec = 3, useSpaces = FALSE) {
    if (!p_is_number(uglyNumber)) {
      return(default)
    }

    if (is.integer(uglyNumber) && !useSpaces) {
      return(sprintf("%d", uglyNumber))
    }

    if (prec == "auto") {
      if (uglyNumber == 0) {
        prec <- 3
      } else {
        prec <- max(0, 3 - floor(log10(abs(uglyNumber))))
      }
    }
    fmt <- sprintf("%%.%df%%s", prec)

    nSpaces <- 0
    if (useSpaces) {
      nSpaces <- ifelse(prec == 0, 4, max(0, 3 - prec))
    }

    # We hate to see -0.0
    uglyNumber <- unlist(uglyNumber, use.names = FALSE)[1]
    uglyNumber[abs(uglyNumber) < 0.1^max(1, prec)] <- 0

    return(sprintf(fmt, uglyNumber, paste(rep(" ", nSpaces), collapse = '')))
  }

p_nof <-
  function (number) {
    if (!p_is_number(number)) {
      return("")
    }
    return(sprintf("(%d)", number))
  }

p_get_precision <-
  function (x) {
    if (!is.finite(x)) {
      return(NA)
    }
    x <- as.character(x)
    if (!grepl(".", x, fixed = TRUE)) {
      return(0)
    }
    frac <- strsplit(x, ".", fixed = TRUE)[[1]][2]
    if (is.na(frac) || nchar(frac) == 0) {
      return(0)
    }
    digits <- strsplit(frac, "", fixed = TRUE)[[1]]
    max(which(digits != "0"))
  }

p_warn_percentage_max <-
  function (loop.data, bn.data) {
    if (p_bottleneck_id(bn.data$bn.y) == 2 && loop.data$scope.theo[3] < 0) {
      warning(paste0("Using bottleneck.y with Y values < 0",
                     ", results might be counterintuitive!"), call. = F)
    }
  }

p_if_min_else_max <-
  function (use.min, ..., na.rm = FALSE) {
    dots <- c(...)
    return(ifelse(use.min, min(dots, na.rm = na.rm), max(dots, na.rm = na.rm)))
  }

p_is_equal <-
  function (value_1, value_2) {
    max.diff <- min(abs(value_1), abs(value_2)) / 1E6
    diff <- abs(value_1 - value_2)
    return(diff <= max.diff)
  }

p_weights <-
  function (loop.data, peers) {
    x <- loop.data$x
    flip.x <- loop.data$flip.x

    weights <- c()
    for (i in 1:(nrow(peers) - 1)) {
      if (!flip.x) {
        count <- x < peers[i + 1, 1]
      } else {
        count <- x > peers[i + 1, 1]
      }
      weights <- c(weights, length(x[count]) - sum(weights))
    }

    # Add the last column
    weights <- c(weights, length(x) - sum(weights))

    return(weights)
  }

p_get_digits <-
  function (tmp) {
    get_max_nchar <- function (n) { nchar(sub("0+$", "", sprintf("%f", n %% 1))) }
    return(min(3, max(sapply(tmp, get_max_nchar) - 2)))
  }

p_start_cluster <-
  function (condition) {
    # Always start cluster when forced
    if (Sys.getenv(p_parallel_force) == TRUE) {
      # Unless already started
      if (Sys.getenv(p_parallel_start) != TRUE) {
        registerDoParallel(p_max_cores())
        p_setenv(p_parallel_start)
      }
    }
    else if (condition) {
      paste("Starting the analysis on", p_max_cores(), "cores")
      if (grepl("windows", tolower(.Platform$OS.type))) {
        cat("Setting up parallelization, this might take a few seconds...")
      }

      # Create cluster for parallisation
      registerDoParallel(p_max_cores())

      if (grepl("windows", tolower(.Platform$OS.type))) {
        cat("\r", strrep(" ", 61), "\r")
      }
    }
    else {
      # Do sequential, this prohibits warnings on %dopar%
      registerDoSEQ()
    }
  }

p_stop_cluster <-
  function () {
    # When forced, don't stop cluster
    if (Sys.getenv(p_parallel_force) != TRUE) {
      stopImplicitCluster()
    }
  }

p_cluster_cleanup <-
  function () {
    # When forced, don't cleanup
    if (Sys.getenv(p_parallel_force) != TRUE) {
      env <- utils::getFromNamespace(".foreachGlobals", "foreach")
      if (!identical(ls(name = env), character(0))) {
        rm(list = ls(name = env), pos = env)
      }
    }
  }

p_max_cores <-
  function () {
    env.max.cores <- as.numeric(Sys.getenv("max.cores", 1000))
    max.cores <- min(env.max.cores, detectCores())
    return(max.cores)
  }

p_dopar_condition <-
  function (condition, min.cores = 2) {
    return(condition && p_max_cores() > min.cores)
  }

p_is_single_string <- function (input) {
  return(is.character(input) & length(input) == 1)
}

p_setenv <-
  function (name, value = TRUE) {
    args <- list(value)
    names(args) <- name
    do.call(Sys.setenv, args)
  }
