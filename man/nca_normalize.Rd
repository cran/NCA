\name{nca_util_normalize}
\alias{nca_util_normalize}
\title{Normalizes numeric data}
\description{(Min-max) Normalize the columns of the data}
\usage{nca_util_normalize(data, scale = NULL, min_max = NULL)}
\arguments{
\item{data}{Input dataframe or matrix.}
\item{scale}{Set(s) of output Min-Max values.}
\item{min_max}{Set(s) of (theoretical) Min-Max values.}
}
\details{
\strong{scale}\cr
If scale is NULL (default), all columns will be normalized between 0 and 1.\cr
Use c(0, 100) to normalize between 0 and 100.\cr
Replicate for each column for different scale, e.g. c(0, 1, 0, 1, 0, 100).\cr
Use NA in the min-max pair if a column does not need to be normalized.\cr
}
\examples{
# Normalize the data between 0 and 1
data(nca.example)
data.normalized <- nca_util_normalize(nca.example)
# Equivalent to this
data.normalized <- nca_util_normalize(nca.example, c(0, 1))

# Normalize the data between 0 and 100
data.normalized <- nca_util_normalize(nca.example, c(0, 100))

# Normalize each column with different values
data.normalized <- nca_util_normalize(nca.example, c(0, 1, 0, 10, 0, 100))

# Use NA if a column doesn't need to be normalized
data.normalized <- nca_util_normalize(nca.example, c(0, 1, NA, NA, 0, 100))

# It is also possible to use a theoretic range for the normalization
data.normalized <- nca_util_normalize(nca.example, min_max = c(0, 100, 0, 150, 0, 300))
}
\keyword{functions}
