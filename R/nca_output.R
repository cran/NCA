nca_output <-
  function (model, plots = TRUE, plotly = FALSE, bottlenecks = FALSE, summaries = TRUE,
            test = FALSE, pdf = FALSE, path = NULL, selection = NULL, plot_bottlenecks = 0) {
    # model needs to be the output of the NCA command

    result <- NULL

    # We have a numeric vector
    if (is.numeric(selection)) {
      selection <- intersect(selection, seq_along(model$summaries))
    }
      # We have a string (names) vector
    else {
      selection <- match(selection, names(model$summaries))
      selection <- selection[!is.na(selection)]
    }
    # Just take all
    if (is.null(selection) || length(selection) == 0) {
      selection <- seq_along(model$summaries)
    }

    if (bottlenecks) {
      # Insert the Y-column as first
      selection.bottlenecks <- c(1, selection + 1)

      bn <- list()
      for (method in names(model$bottlenecks)) {
        bn.method <- model$bottlenecks[[method]]
        bn[[method]] <- bn.method[, selection.bottlenecks]

        atts <- c("bn.x", "bn.y", "bn.y.id", "size", "cutoff")
        for (att in atts) {
          attr(bn[[method]], att) <- attr(model$bottlenecks[[method]], att)
        }
      }
      p_display_bottleneck(bn, pdf = pdf, path = path)
    }

    if (summaries) {
      for (i in selection) {
        summary <- model$summaries[[i]]
        p_display_summary(summary, pdf = pdf, path = path)
      }
    }

    if (plots) {
      for (i in selection) {
        plot <- model$plots[[i]]
        bottleneck.list <- p_get_bottleneck_list(model, i, plot_bottlenecks)
        p <- p_display_plot(plot, bottleneck.list = bottleneck.list, pdf = pdf, path = path)
        name <- names(model$summaries)[i]
        result$plots[[name]] <- p
      }
    }

    if (test) {
      if (length(model$tests) == 0) {
        warning(paste("Tests are selected in the output,",
                      "but non were supplied by the analysis!"), call. = F)
      }
      else {
        for (i in selection) {
          test <- model$tests[[i]]
          p_display_test(test, pdf = pdf, path = path)
        }
      }
    }

    if (!isFALSE(plotly)) {
      for (i in selection) {
        labels <- NULL
        if (!isTRUE(plotly)) {
          labels <- plotly
        }
        peers <- p_aggregate_peers(model$peers, i)
        bottleneck.list <- p_get_bottleneck_list(model, i, plot_bottlenecks)
        p_display_plotly(model$plots[[i]], peers, labels, bottleneck.list = bottleneck.list)
      }
    }

    cat("")
    invisible(result)
  }

p_get_bottleneck_list <-
  function (model, i, plot_bottlenecks) {
    bottleneck.list <- list(ver = list(), hor = list())
    if (plot_bottlenecks %in% c(1, 2)) {
      mpy <- attr(model$bottlenecks, "mpy")
      mpy_labels <- model$bottlenecks[[1]][, 1]
      precision <- median(sapply(mpy_labels, p_get_precision), na.rm = TRUE)
      precision <- ifelse(is.na(precision), 0, precision)
      x0 <- model$plots[[i]]$scope.theo[1]
      x1 <- model$plots[[i]]$scope.theo[2]
      for (j in seq_along(mpy)) {
        label <- format(round(mpy_labels[j], precision), nsmall = precision)
        coord <- list(x0, x1, mpy[j], mpy[j])
        bottleneck.list[['hor']][[label]] <- coord
      }

      # Aggregate over all methods, make sure the lines are unique
      if (plot_bottlenecks == 2) {
        y0 <- model$plots[[i]]$scope.theo[3]
        y1 <- model$plots[[i]]$scope.theo[4]

        for (method in names(model$bottlenecks)) {
          tmp <- model$bottlenecks[[method]][, i + 1]
          mpx.actual <- attr(tmp, "mpx.actual")
          tmp <- suppressWarnings(as.numeric(tmp))
          precision <- median(sapply(tmp, p_get_precision), na.rm = TRUE)
          precision <- ifelse(is.na(precision), 0, precision)
          for (j in seq_len(nrow(mpx.actual))) {
            if (is.finite(mpx.actual[j])) {
              value <- ifelse(is.na(tmp[j]), 0, tmp[j])
              label <- format(round(value, precision), nsmall = precision)
              coord <- list(mpx.actual[j], mpx.actual[j], y0, y1)
              bottleneck.list[['ver']][[label]] <- coord
            }
          }
        }
      }
    }
    return(bottleneck.list)
  }

print.nca_result <-
  function (x, ...) {
    p_display_summary_simple(x$summaries)
    if (attr(x, "show.plots")) {
      nca_output(x, plots = TRUE, summaries = FALSE, bottlenecks = FALSE)
    }
  }

summary.nca_result <-
  function (object, columns = NULL, ...) {
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
    nca_output(x, plots = TRUE, summaries = FALSE, bottlenecks = FALSE)
  }
