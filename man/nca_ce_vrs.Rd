\name{nca_ce_vrs}
\alias{nca_ce_vrs}
\title{Run ceiling envelopment VRS analyses on a data set}
\description{Run ceiling envelopment VRS analyses on a data set}
\usage{nca_ce_vrs(loop.data, ...)}
\arguments{
  \item{loop.data}{data object for this analysis}
  \item{...}{ignored}
}
\value{Returns a list for the CE VRS analysis with:\cr
  \item{line}{line of the calculated ceiling}
  \item{ceiling}{ceiling zone}
  \item{effect}{effect size}
  \item{bottleneck}{bottleneck data}
}
\keyword{functions}