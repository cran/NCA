p_generate_title <-
function (x.name, y.name) {
  return (paste(x.name, y.name, sep = " - "))
}

p_pretty_name <-
function (uglyName) {
  return( gsub("_", "-", toupper(uglyName)) )
}

p_is_number <-
function(number) {
  if (typeof(number) == "list") {
    un_list = unlist(number, use.names=FALSE)
    if (is.null(un_list) || is.infinite(un_list)) {
      return (FALSE)
    }
  }

  if (is.null(number) || is.na(number) || number == "NA" || number == "NN") {
    return(FALSE)
  } else {
    return(TRUE)
  }
}

p_pretty_number <-
function (uglyNumber, default="", prec=3, useSpaces=FALSE) {
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
    nSpaces <- ifelse(prec == 0, 4, max(0, 3-prec))
  }

  # We hate to see -0.0
  uglyNumber <- unlist(uglyNumber, use.names=FALSE)[1]
  uglyNumber[abs(uglyNumber) < 0.1 ^ max(1, prec)] <- 0

  return(sprintf(fmt, uglyNumber, paste(rep(" ", nSpaces), collapse='')))
}

p_warn_percentage_max <-
function (loop.data, bn.data) {
  if (p_bottleneck_id(bn.data$bn.y) == 2 && loop.data$scope.theo[3] < 0) {
    message("")
    message(paste0("Warning : using bottleneck.y with Y values < 0",
                   ", results might be counterintuitive!"))
    message("")
  }
}

p_if_min_else_max <-
function (use.min, ..., na.rm=FALSE) {
  dots <- c(...)
  return( ifelse(use.min, min(dots, na.rm=na.rm), max(dots, na.rm=na.rm)) )
}

p_weights <-
function (loop.data, peers) {
  x <- loop.data$x
  flip.x <- loop.data$flip.x

  weights <- c()
  for (i in 1:(nrow(peers)-1)) {
    if (!flip.x) {
      count <- x < peers[i+1, 1]
    } else {
      count <- x > peers[i+1, 1]
    }
    weights <- c(weights, length(x[count]) - sum(weights))
  }

  # Add the last column
  weights <- c(weights, length(x) - sum(weights))

  return( weights )
}

print.nca_result <-
function (x, ...) {
  p_display_summary_simple(x$summaries)
  if (attr(x, "show.plots")) {
    for (plot in x$plots) {
      p_display_plot(plot)
    }
  }
}

summary.nca_result <-
function (object, columns=NULL, ...) {
  if (!is.null(columns)) {
    # Columns can be indexes or names
    if (!is.numeric(columns)) {
      columns <- match(c(columns), names(object$summaries))
      columns <- columns[!is.na(columns)]
    }
    else {
      columns <- columns[columns > 0]
      columns <- columns[columns < length(object$summaries)]
    }

    # Make sure user actually selected columns
    tmp <- object$summaries[columns]
    if (length(tmp) > 0) {
      object$summaries <- tmp
    }
  }

  nca_output(object)
}

plot.nca_result <-
function (x, ...) {
  nca_output(x, plots=TRUE, summaries=FALSE, bottlenecks=FALSE)
}

p_get_digits <-
function (tmp) {
  get_max_nchar <- function (n) { nchar(sub("0+$", "", sprintf("%f", n %% 1))) }
  return( min(3, max(sapply(tmp, get_max_nchar) - 2)) )
}

p_accuracy <-
function (loop.data, above) {
  nObservations <- min(length(loop.data$x), length(loop.data$y))
  return (100 * (nObservations - above) / nObservations)
}
