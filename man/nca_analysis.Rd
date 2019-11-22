\name{nca_analysis}
\alias{nca_analysis}
\title{Run NCA analyses on a data set}
\description{Run multiple types of NCA analyses on a dataset}
\usage{nca_analysis(data, x, y, ceilings=c('ols', 'ce_fdh', 'cr_fdh'),
             flip.x=FALSE, flip.y=FALSE, scope=NULL,
             bottleneck.x='percentage.range', bottleneck.y='percentage.range',
             steps=10, step.size=NULL, cutoff=0, qr.tau=0.95, effect_aggregation = c(1),
             test.rep=0, test.p_confidence=0.95, test.p_threshold=0)}
\arguments{
\item{data}{dataframe with columns of the variables}
\item{x}{index or name (or a vector of those) with independent variable(s)}
\item{y}{index or name of the column with the dependent variable}
\item{ceilings}{vector with the ceiling techniques to include in this analysis}
\item{flip.x}{reverse the direction of the independent variables\cr
 use either a boolean for all independent variables, or a vector with the same length as x}
\item{flip.y}{reverse the direction of the dependent variables, boolean}
\item{scope}{a theoretical scope in list format : (x.low, x.high, y.low, y.high)}
\item{bottleneck.x}{options for displaying the independent variables in the bottleneck table\cr
 'percentage.range' to display the percentage of range between min(x) and max(x)\cr
 'percentage.max' to display the percentage of max(x)\cr
 'actual' to display the actual values\cr
 'percentile' to display the percentiles\cr
 Using percentage.max with negative values might yield counterintuitive results}
\item{bottleneck.y}{options for displaying the dependent variables in the bottleneck table, \cr see bottleneck.x}
\item{steps}{number of steps in the bottleneck table (only useful if step.size is not defined)}
\item{step.size}{define the step size in the bottleneck table\cr
  the user will be warned if the stepsize does not fit the Y range\cr
  defaults to null for using steps}
\item{cutoff}{display calculated x,y values that are lower/higher than lowest/highest observed x,y values in the bottleneck table as:\cr
0 : NN (not necessary) and NA (not available)\cr 1 : lowest/highest observed values\cr 2 : calculated values}\cr
\item{qr.tau}{define the qr tau (between 0 and 1), default 0.95}\cr
\item{effect_aggregation}{define the corners to aggregate into the effect size. 1 is upper-left and is always selected, 2 is upper-right, 3 is lower-left and 4 is lower-right}\cr
\item{test.rep}{number of resamples in the approximate permutation test}\cr
\item{test.p_confidence}{confidence level of the estimated p-value \cr
will be used to calculate the p-accuracy for a given number of samples (test.rep), default 0.95}\cr
\item{test.p_threshold}{define the threshold significance level in the returned plot of the approximate permutation test, default 0 (returns nothing)}\cr
}

\value{Returns a list of 6 items (see examples for further explanation):
\item{plots}{A list of plot-data each x-y combination}
\item{summaries}{A list of dataframes with the summaries for each x-y combination}
\item{bottlenecks}{A list of dataframes with a bottleneck table for each ceiling technique}
\item{peers}{A list of peers for each independent variable}
\item{tests}{The results of the test for each independent variable, if test.rep is larger than 0 (not human friendly, use nca_output)}
\item{test.time}{The total time needed to run the tests for all independent variables}
}
\examples{
# Load the data
data(nca.example)
data <- nca.example

# Basic NCA analysis, with independent variables in the first 2 columns
# and the dependent variable in the third column
model <- nca_analysis(data, c(1, 2), 3)

# Use nca_output to show the summaries (see nca_output documentation for more options)
nca_output(model)

# Columns can be selected by name as well
model <- nca_analysis(data, c('Individualism', 'Risk taking'), 'Innovation performance')

# Define the ceiling techniques via the ceilings parameter, see 'ceilings' for all types
model <- nca_analysis(data, c(1, 2), 3, ceilings=c('ce_fdh', 'ce_vrs'))
# These are the available ceiling techniques
print(ceilings)

# For using the upper right corner(s), 'flip' the x variables
model <- nca_analysis(data, c(1, 2), 3, flip.x=TRUE)
# It is also possible to flip a single x variable
model <- nca_analysis(data, c(1, 2), 3, flip.x=c(TRUE, FALSE))
# Flip the y variable if the lower corners need analysing
model <- nca_analysis(data, c(1, 2), 3, flip.x=c(TRUE, FALSE), flip.y=TRUE)

# Use a theoretical scope instead of the (calculated) empirical scope
model <- nca_analysis(data, c(1, 2), 3, scope=c(0, 120, 0, 240))

# By default the bottleneck tables use percentages of the range for the x and y values.
# Using the percentage of the max value is also possible
model <- nca_analysis(data, c(1, 2), 3, bottleneck.y='percentage.max')
# Use the actual values, in this case the x-value
model <- nca_analysis(data, c(1, 2), 3, bottleneck.x='actual')
# Use percentile, in this case for the y-values
model <- nca_analysis(data, c(1, 2), 3, bottleneck.y='percentile')
# Any combination is possible
model <- nca_analysis(data, c(1, 2), 3, bottleneck.x='actual', bottleneck.y='percentile')

# The number of steps is adjustible via the steps parameter
model <- nca_analysis(data, c(1, 2), 3, steps=20)
# Or via the step.size parameter, this ignores the steps parameter
model <- nca_analysis(data, c(1, 2), 3, step.size=5)

# If the ceiling line crosses the X = Xmax line at a point C below Y = Ymax,
# for Y < Yc < Ymax, the corresponding X in the bottleneck table is displayed as 'NA'
# It is also possible to display them as Xmax
model <- nca_analysis(data, c(1, 2), 3, cutoff=1)
# or as the calculated value on the ceiling line
model <- nca_analysis(data, c(1, 2), 3, cutoff=2)

# To run tests, the test.rep needs to be larger than 0
# Optionally the p_confidence (default 0.95) and the p_threshold (default 0) can be set
\donttest{model <- nca_analysis(data, c(1, 2), 3, test.rep=1000, test.p_confidence=0.9, test.p_threshold=0.05)}
\dontshow{model <- nca_analysis(data, c(1, 2), 3, test.rep=100, test.p_confidence=0.9, test.p_threshold=0.05)}

# The output of the tests can be shown via nca_output with test=TRUE
nca_output(model, test=TRUE)
}
\keyword{functions}
