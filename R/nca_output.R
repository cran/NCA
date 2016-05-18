nca_output <-
function (model, plots=FALSE, bottlenecks=FALSE, summaries=TRUE,
          pdf=FALSE, path=NULL) {
  # model needs to be the output of the NCA command

  if (plots) {
    for(plot in model$plots) {
      p_display_plot(plot, pdf=pdf, path=path)
    }
  }

  if (bottlenecks) {
    p_display_bottleneck(model$bottlenecks, pdf=pdf, path=path)
  }

  if (summaries) {
    for (summary in model$summaries) {
      p_display_summary(summary, pdf=pdf, path=path)
    }
  }

  cat("")
}
