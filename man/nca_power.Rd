\name{nca_power}
\alias{nca_power}
\title{Function to evaluate power}
\description{Function to evaluate power,
test if a sample size is large enough to detect necessity.}
\usage{nca_power(n = c(20, 50, 100), effect = 0.10, slope = 1, ceiling = "ce_fdh",
corner = 1, p = 0.05, distribution.x = "uniform", distribution.y = "uniform",
rep = 100, test.rep = 200)}
\arguments{
\item{n}{Number of datapoints to generate, either an integer or a vector of integers.}
\item{effect}{Effect size of the generated datasets (single value or vector of values).}
\item{slope}{Slope of the line (single value or vector of values).}
\item{ceiling}{Ceiling technique to use for this analysis (single value or vector of values).}
\item{corner}{Integer, indicating the corner to analyze, see nca_analysis.}
\item{p}{Significance level.}
\item{distribution.x}{Distribution type(s) for X, "uniform" (default) or "normal" (single value or vector of values).}
\item{distribution.y}{Distribution type(s) for Y, "uniform" (default) or "normal" (single value or vector of values).}
\item{rep}{Number of analyses done per iteration.}
\item{test.rep}{Number of resamples in the statistical approximate permutation test.
For test.rep = 0 no statistical test is performed. See \code{\link{nca_analysis}}
for reproducible outputs.}
}
\examples{
# Simple example
\dontrun{results <- nca_power()}
\dontshow{results <- nca_power(rep=1, test.rep = 1)}
print(results)
}
\keyword{functions}
