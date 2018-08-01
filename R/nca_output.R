nca_output <-
function (model, plots=FALSE, bottlenecks=FALSE, summaries=TRUE, test=FALSE,
          pdf=FALSE, path=NULL) {
  # model needs to be the output of the NCA command

  if (bottlenecks) {
    p_display_bottleneck(model$bottlenecks, pdf=pdf, path=path)
  }

  if (summaries) {
    for (summary in model$summaries) {
      p_display_summary(summary, pdf=pdf, path=path)
    }
  }

  if (plots) {
    for (plot in model$plots) {
      p_display_plot(plot, pdf=pdf, path=path)
    }
  }

  if (test) {
    if (length(model$tests) == 0) {
      message(paste("\nTests are selected in the output,",
                    "but non were supplied by the analysis!\n"))
    }
    for (test in model$tests) {
      p_display_test(test, pdf=pdf, path=path)
    }
  }

  cat("")
}
