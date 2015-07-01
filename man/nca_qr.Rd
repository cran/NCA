\name{nca_qr}
\alias{nca_qr}
\title{Run QR analyses on a data set}
\description{Run Quantile Regression analyses on a data set}
\usage{nca_qr(loop.data, ...)}
\arguments{
  \item{loop.data}{data object for this analysis}
  \item{...}{ignored}
}
\value{Returns a list for the QR analysis with:\cr
  \item{line}{line of the calculated ceiling}
  \item{ceiling}{ceiling zone}
  \item{slope}{slope of the ceiling line}
  \item{intercept}{intercept of the ceiling line}
  \item{effect}{effect size}
  \item{above}{number of observations above the ceiling line}
  \item{ineffs}{condition and outcome inefficiencies}
}
\keyword{functions}