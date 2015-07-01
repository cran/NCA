\name{nca_ols}
\alias{nca_ols}
\title{Run OLS analyses on a data set}
\description{Run OLS analyses on a data set}
\usage{nca_ols(loop.data, ...)}
\arguments{
  \item{loop.data}{data object for this analysis}
  \item{...}{ignored}
}
\value{Returns a list for the OLS analysis with:\cr
  \item{line}{line of the calculated ceiling}
  \item{ceiling}{ceiling zone}
  \item{slope}{slope of the ceiling line}
  \item{intercept}{intercept of the ceiling line}
  \item{effect}{effect size}
  \item{above}{number of observations above the ceiling line}
  \item{ineffs}{condition and outcome inefficiencies}
}
\keyword{functions}