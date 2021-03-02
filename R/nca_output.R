nca_output <-
function (model, plots=FALSE, plotly=FALSE, bottlenecks=FALSE, summaries=TRUE,
          test=FALSE, pdf=FALSE, path=NULL, selection=NULL) {
  # model needs to be the output of the NCA command

  # We have a numeric vector
  if (is.numeric(selection)) {
    selection <- intersect(selection, 1:length(model$summaries))
  }
  # We have a string (names) vector
  else {
    selection <- match(selection, names(model$summaries))
    selection <- selection[!is.na(selection)]
  }
  # Just take all
  if (is.null(selection) || length(selection) == 0) {
    selection <- 1:length(model$summaries)
  }

  if (bottlenecks) {
    bn <- list()
    for (method in names(model$bottlenecks)) {
      # Insert the Y-column as first
      selection.bottlenecks <- c(1, selection + 1)

      bn.method <- model$bottlenecks[[method]]
      bn[[method]] <- bn.method[, selection.bottlenecks]

      atts <- c("bn.x", "bn.y", "bn.y.id", "size", "cutoff")
      for (att in atts) {
        attr(bn[[method]], att) <- attr(model$bottlenecks[[method]], att)
      }
    }
    p_display_bottleneck(bn, pdf=pdf, path=path)
  }

  if (summaries) {
    for (i in selection) {
      summary <- model$summaries[[i]]
      p_display_summary(summary, pdf=pdf, path=path)
    }
  }

  if (plots) {
    for (i in selection) {
      plot <- model$plots[[i]]
      p_display_plot(plot, pdf=pdf, path=path)
    }
  }

  if (test) {
    if (length(model$tests) == 0) {
      message(paste("\nTests are selected in the output,",
                    "but non were supplied by the analysis!\n"))
    }
    else {
      for (i in selection) {
        test <- model$tests[[i]]
        p_display_test(test, pdf=pdf, path=path)
      }
    }
  }

  if (!isFALSE(plotly)) {
    for (i in selection) {
      labels <- NULL
      if (!isTRUE(plotly)) {
        labels <- plotly
      }
      p_display_plotly(model$plots[[i]], model$peers[[i]], labels)
    }
  }

  cat("")
}
