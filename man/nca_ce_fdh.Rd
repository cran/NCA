\name{nca_ce_fdh}
\alias{nca_ce_fdh}
\title{Run ceiling envelopment FDH analysis on a data set}
\description{Run ceiling envelopment FDH analysis on a data set}
\usage{nca_ce_fdh(loop.data, mpy, cutoff, bottleneck.x, fast.fdh=TRUE)}
\arguments{
  \item{loop.data}{data object for this analysis}
  \item{mpy}{dependent data for bottleneck calculation}
  \item{cutoff}{show calculated x,y values that are lower/higher than lowest/highest observed x,y values in the bottleneck table as:\cr
  0 : NA\cr 1 : lowest/highest observed values\cr 2 : calculated values}
  \item{bottleneck.x}{options for displaying the independent variables in the bottleneck table}
  \item{fast.fdh}{use our own implementation for finding FDH peers, else via DEA}
}
\value{Returns a list for the CE FDH analysis with:\cr
  \item{line}{line of the calculated ceiling}
  \item{ceiling}{ceiling zone}
  \item{effect}{effect size}
  \item{ineffs}{condition and outcome inefficiencies}
  \item{bottleneck}{bottleneck data}
}
\keyword{functions}