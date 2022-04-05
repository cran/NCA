\name{NCA-package}
\docType{package}
\title{Necessary Condition Analysis}
\description{The NCA package implements Necessary Condition Analysis (NCA) as developed by Dul (2016). For running the NCA package a data file (e.g., mydata.csv, which contains the input data) must be available. An example data file (presented in above article) is included in the package. The user must load the data and call the nca function.}
\details{\tabular{ll}{
Package:  \tab NCA\cr
Type:     \tab Package\cr
Version:  \tab 3.2.0\cr
Date:     \tab 2022-04-04\cr
License:  \tab GPL (>= 3)\cr}
}
\author{
Author: Jan Dul \email{jdul@rsm.nl}\cr
Maintainer: Govert Buijs \email{buijs@rsm.nl}
}
\references{
Dul, J. 2016. Necessary Condition Analysis (NCA).Logic and methodology of 'necessary but not sufficient' causality.
Organizational Research Methods 19(1), 10-52.
\href{https://journals.sagepub.com/doi/full/10.1177/1094428115584005}{https://journals.sagepub.com/doi/full/10.1177/1094428115584005}\cr
Dul, J. (2020). Conducting Necessary Condition Analysis. Sage publishers. ISBN: 9781526460141.
\href{https://uk.sagepub.com/en-gb/eur/conducting-necessary-condition-analysis-for-business-and-management-students/book262898}{https://uk.sagepub.com/en-gb/eur/conducting-necessary-condition-analysis-for-business-and-management-students/book262898}\cr
Dul, J., van der Laan, E., & Kuik, R. (2020). A statistical significance test for Necessary Condition Analysis. Organizational Research Methods, 23(2), 385-395.
\href{https://journals.sagepub.com/doi/10.1177/1094428118795272}{https://journals.sagepub.com/doi/10.1177/1094428118795272}

}
\seealso{
\code{\link{nca_analysis}}, \code{\link{nca_output}}
}
\keyword{package}

\examples{
# A more detailed guide can be found here : https://repub.eur.nl/pub/78323/
# or https://papers.ssrn.com/sol3/papers.cfm?abstract_id=2624981

# Load data from a CSV file with header and row names:
\donttest{\dontshow{try(}data <- read.csv('mydata.csv', row.names=1)\dontshow{, silent=TRUE)}}
# Or load and rename the example dataset
data(nca.example)
data <- nca.example

# Run NCA with the dataset and name the analysis 'model'.
# Specify the independent (cause) and dependent (effect) variables by column index or name
# More than 1 independent variables can be specified with a vector
model <- nca_analysis(data, c(1, 2), 3)

# A quick summary of the analysis can be displayed by 'model'
model

# A full summary of the analysis is shown by nca_output (see documentation for more options)
nca_output(model)

# The results of the analysis is a list of 6 items :
# - plots (1 for each independent variable)
# - summaries (1 for each independent variable)
# - bottleneck tables (1 for each ceiling technique)
# - peers (1 dataframe for each independent variable)
# - tests (1 list for each independent variabl)
# - test.time (total time to run all tests)
names(model)

# The first item contains the graphical outputs for each independent variable
# This is not really useful to humans
model$plots[[1]]

# The seconds item contains a list with the summaries for the independent variables
model$summaries[[1]]

# The third item contains a list with the bottleneck tables, one for each ceiling technique
model$bottlenecks$cr_fdh

# The fourth item shows the peers, for each independent variable
model$peers$Individualism

# For the fifth and sixth item, the test.rep needs to be larger than 0
# for performing the statistical test
# Optionally the p_confidence (default 0.95) and the p_threshold (default 0) can be set
model <- nca_analysis(data, c(1, 2), 3, test.rep=100)

# The fifth item shows the tests for each independent variable
# This is not really useful to humans
model$tests$Individualism

# The last item shows the total time needed to perform the analysis.
# For large values of test.rep the test may take long.  
model$test.time
}
