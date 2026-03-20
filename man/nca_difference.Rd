\name{nca_difference}
\alias{nca_difference}
\title{Function for permutation tests}
\description{Function for permutation tests}
\usage{nca_difference(data1, data2 = NULL, x, y,
ceilings = c("ce_fdh", "cr_fdh"), scope = NULL, test.rep = 1000,
test.type = c("contrast", "independent", "paired"))}
\arguments{
\item{data1}{Dataframe 1.}
\item{data2}{Dataframe 2, defaults to NULL.}
\item{x}{Index or name (or a vector of those) with condition(s) x}
\item{y}{Index or name of the column with the outcome y}
\item{ceilings}{Vector with the ceiling techniques to test}
\item{scope}{A theoretical scope in list format : (x.low, x.high, y.low, y.high), see nca_analysis}
\item{test.rep}{Number of permutations to use, defaults to 1000.}
\item{test.type}{Name of the test type: "contrast", "independent" or "paired".}
}
\examples{
ceilings <- "cr_fdh"
y <- "Y"
test.rep <- 1000
\dontshow{test.rep = 1}
# Contrast test: effect size difference between two conditions
test.type <- "contrast"
data1 <- nca_random(100, c(0.2,0.4), c(1,1))
x <- c("X1", "X2")

difference <- nca_difference(data1 = data1, x = x, y = y,
                             ceilings = ceilings, test.rep = test.rep,
                             test.type = test.type)
print(difference)

# Independent test: effect size difference between two datasets
test.type <- "independent"
data1 <- nca_random(100, 0.2, 1)
data2 <- nca_random(100, 0.4, 1)
x <- "X"

difference <- nca_difference(data1 = data1, data2 = data2, x = x, y = y,
                             ceilings = ceilings, test.rep = test.rep,
                             test.type = test.type)
print(difference)

# paired test: effect size differences between 2 measurements
test.type <- "paired"
data1 <- nca_random(100, 0.2, 1)
data2 <- nca_random(100, 0.4, 1)
x <- "X"

difference <- nca_difference(data1 = data1, data2 = data2, x = x, y = y,
                             ceilings = ceilings, test.rep = test.rep,
                             test.type = test.type)
print(difference)
}
\keyword{functions}
