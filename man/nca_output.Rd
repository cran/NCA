\name{nca_output}
\alias{nca_output}
\title{display the result of the NCA analysis}
\description{Show the plots, NCA summaries and bottlenecks of a NCA analysis.}
\usage{nca_output(model, plots=FALSE, bottlenecks=FALSE, summaries=TRUE,
                  pdf=FALSE, path=NULL)}
\arguments{
  \item{model}{the output of the nca or nca_analysis commands}
  \item{plots}{if true show the plot(s)}
  \item{bottlenecks}{if true show the bottleneck table(s)}
  \item{summaries}{if true show the summaries}
  \item{pdf}{if true export to pdf}
  \item{path}{optional path for the output file(s)}
}
\examples{
  # Use the result of the nca command :
  data(nca.example)
  data <- nca.example
  model <- nca_analysis(data, c(1, 2), 3)

  # This shows the summaries in the console
  nca_output(model)

  # Suppress the summaries and display the plots
  nca_output(model, plots=TRUE, summaries=FALSE)

  # Suppress the summaries and display the bottlenecks
  nca_output(model, bottlenecks=TRUE, summaries=FALSE)

  # Show all three
  nca_output(model, plots=TRUE, bottlenecks=TRUE)

  # Export to PDF, the plots and summaries each to a file per independent variable
  # All the bottlenecks will exported to a single file
  nca_output(model, plots=TRUE, bottlenecks=TRUE, pdf=TRUE)

  # Use the path option to export to an existing directory
  outdir <- "/tmp" \dontshow{outdir <- tempdir()}
  nca_output(model, plots=TRUE, pdf=TRUE, path=outdir)
}
\keyword{functions}
