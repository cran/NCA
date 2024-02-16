\name{nca_random}
\alias{nca_random}
\title{generating random data that meets necessity}
\description{Generate N datapoints, with 'normal' or 'uniform' distributions for X and Y}
\usage{nca_random(n, intercepts, slopes, corner=1,
                  distribution.x = "uniform", distribution.y = "uniform",
                  mean.x = 0.5, mean.y = 0.5, sd.x = 0.2, sd.y = 0.2)}
\arguments{
\item{n}{Number of observations to generate, should be an integer > 1.}
\item{intercepts}{The intercept or a vector of intercepts of the line.}
\item{slopes}{The slope or a vector if slopes of the line.}
\item{corner}{Define which corner should be empty, default is 1 (upper left).}
\item{distribution.x}{Type of the distribution for X, "uniform" (default) or "normal".\cr
The latter is a truncated normal distribution.}
\item{distribution.y}{Type of the distribution for Y, "uniform" (default) or "normal".\cr
The latter is a truncated normal distribution.}
\item{mean.x}{Distribution Mean of X (default 0.5), ignored distribution.x == "uniform".}
\item{mean.y}{Distribution Mean of Y (default 0.5), ignored distribution.y == "uniform".}
\item{sd.x}{Distribution SD of X (default 0.2), ignored distribution.x == "uniform".}
\item{sd.y}{Distribution SD of Y (default 0.2), ignored distribution.y == "uniform".}
}
\examples{
# Generate a uniform dataset, default for X and Y
data <- nca_random(100, 0, 1)

# It is also possible to generate a dataset with multiple independent variables,
# by supplying vectors for the intercepts and slopes
data <- nca_random(100, c(0, 0.25), c(1, 0.75))
# Single values will be repeated to complement a vector  
data <- nca_random(100, c(0, 0.25), 1)

# The default is an empty space in the upper left corner.
# A different corner can be selected with the corner argument
data <- nca_random(100, 0, 1, corner=4)

# Generate a dataset with a normal distribution for X and a uniform distribution for Y
data <- nca_random(100, 0, 1, distribution.x = "normal", distribution.y = "uniform")

# Generate a dataset with a normal distribution for X and Y, with adjusted MEAN
data <- nca_random(100, 0, 1, distribution.x = "normal", distribution.y = "normal",
                   mean.x = 0.75, mean.y = 0.75)

# Generate a dataset with a normal distribution for X and Y, with adjusted SD
data <- nca_random(100, 0, 1, distribution.x = "normal",
                   distribution.y = "normal", sd.x = 0.1, sd.y = 0.1)
}
\keyword{functions}

