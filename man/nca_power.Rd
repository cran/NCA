\name{nca_power}
\alias{nca_power}
\title{Function to evaluate power}
\description{Function to evaluate power,
test if a sample size is large enough to detect necessity.}
\usage{nca_power(n = c(20, 50, 100), effect = 0.10, slope = 1, ceiling = "ce_fdh",
p = 0.05, distribution.x = "uniform", distribution.y = "uniform", rep = 100,
test.rep = 200)}
\arguments{
\item{n}{Number of datapoints to generate, either an integer or a vector of integers.}
\item{effect}{Effect size of the generated datasets.}
\item{slope}{Slope of the line.}
\item{ceiling}{Ceiling technique to use for this analysis}
\item{p}{Targeted confidence level}
\item{distribution.x}{Distribution type(s) for X, "uniform" (default) or "normal".}
\item{distribution.y}{Distribution type(s) for Y, "uniform" (default) or "normal".}
\item{rep}{Number of analyses done per iteration.}
\item{test.rep}{Number of resamples in the statistical approximate permutation test.
For test.rep = 0 no statistical test is performed}
}
\examples{
# Simple example
\dontrun{results <- nca_power()}
\dontshow{results <- nca_power(rep=1, test.rep = 1)}
print(results)
}
\keyword{functions}
