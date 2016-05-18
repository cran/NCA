\name{nca_analysis}
\alias{nca_analysis}
\title{Run NCA analyses on a data set}
\description{Run multiple types of NCA analyses on a dataset}
\usage{nca_analysis(data, x, y, ceilings=c("ols", "ce_fdh", "cr_fdh"),
                    flip.x=FALSE, flip.y=FALSE, scope=NULL, weighting=FALSE,
                    bottleneck.x="percentage.range", bottleneck.y="percentage.range",
                    steps=10, step.size=NULL, cutoff=0)}
\arguments{
  \item{data}{dataframe with columns of the variables}
  \item{x}{index or name (or a vector of those) with independent variable(s)}
  \item{y}{index or name of the column with the dependent variable}
  \item{ceilings}{vector with the method types to include in this analysis}
  \item{flip.x}{reverse the direction of the independent variables\cr
   use either a boolean for all indep. variables, or a vector with the same length as x}
  \item{flip.y}{reverse the direction of the dependent variables, boolean}
  \item{scope}{use a theoretical scope in list format : (x.low, x.high, y.low, y.high)}
  \item{weighting}{set to true for weighted FDH and VRS regressions lines}
  \item{bottleneck.x}{options for displaying the independent variables in the bottleneck table\cr see bottleneck.y}
  \item{bottleneck.y}{options for displaying the dependent variables in the bottleneck table, \cr
   "percentage.range" to display the percentage of range between min(y) and max(y)\cr
   "percentage.max" to display the percentage of max(y)\cr
   "actual" to display the actual values\cr
   "percentile" to display the percentiles\cr
   Using percentage.max with negative values might yield counterintuitive results}
  \item{steps}{number of steps in the bottleneck table (only useful if step.size is not defined)}
  \item{step.size}{define the step size in the bottleneck table\cr
    the user will be warned if the stepsize doesn't fit the Y range\cr
    defaults to null for using steps}
  \item{cutoff}{display calculated x,y values that are lower/higher than lowest/highest observed x,y values in the bottleneck table as:\cr
  0 : NN (not necessary) and NA (not available)\cr 1 : lowest/highest observed values\cr 2 : calculated values}
}
\value{Returns a list with 3 items (see examples for further explanation):\cr
  \item{plots}{A list of plot-data each x-y combination}
  \item{summaries}{A list of dataframes with the summaries for each x-y combination}
  \item{bottlenecks}{A list of dataframes with a bottleneck table for each ceiling technique}
}
\examples{
  # Load the data
  data(nca.example)
  data <- nca.example

  # Basic usage of the nca analysis, with independent variables in the first 2 columns
  # and the dependent variable in the third column
  model <- nca_analysis(data, c(1, 2), 3)

  # Use nca_output to shows the summaries (see nca_output documentation for more options)
  nca_output(model)

  # Columns can be selected by name as well
  model <- nca_analysis(data, c("Individualism", "Risk taking"), "Innovation performance")

  # Define the ceiling techniques via the ceilings parameter, see 'ceilings' for all types
  model <- nca_analysis(data, c(1, 2), 3, ceilings=c("ols", "ce_vrs"))
  # These are the available ceiling techniques
  print(ceilings)

  # For using the upper right corner(s), 'flip' the x variables
  model <- nca_analysis(data, c(1, 2), 3, flip.x=TRUE)
  # Or just for a single x variable
  model <- nca_analysis(data, c(1, 2), 3, flip.x=c(TRUE, FALSE))
  # Flip the y variable if the lower corners need analysing
  model <- nca_analysis(data, c(1, 2), 3, flip.x=c(TRUE, FALSE), flip.y=TRUE)

  # Use a theoretical scope instead of the (calculated) empirical scope
  model <- nca_analysis(data, c(1, 2), 3, scope=c(0, 120, 0, 240))

  # By default the bottlenecks use percentages of the range for the x and y values.
  # But using the percentage of the max value is also possible
  model <- nca_analysis(data, c(1, 2), 3, bottleneck.y="percentage.max")
  # Either can be set to use the actual values, in this case the x-value
  model <- nca_analysis(data, c(1, 2), 3, bottleneck.x="actual")
  # Or use percentile, in this case for the y-values
  model <- nca_analysis(data, c(1, 2), 3, bottleneck.y="percentile")
  # Of course, any combination is possible
  model <- nca_analysis(data, c(1, 2), 3, bottleneck.x="actual", bottleneck.y="percentile")

  # The number of steps is adjustible via the steps parameter
  model <- nca_analysis(data, c(1, 2), 3, steps=20)
  # Or via the step.size parameter, this ignores the steps parameter
  model <- nca_analysis(data, c(1, 2), 3, step.size=5)

  # Values that are outside the scope are translated to 'NA' in the bottleneck tables
  # But we can also set the them to the limits set by the scope
  model <- nca_analysis(data, c(1, 2), 3, cutoff=1)
  # Or use the calculated values on the ceiling line
  model <- nca_analysis(data, c(1, 2), 3, cutoff=2)
}
\keyword{functions}
