\name{nca}
\alias{nca}
\title{Run a basic NCA analyses on a data set}
\description{Run a basic NCA analyses on a data set}
\usage{nca(data, x, y, ceilings=c('ols', 'ce_fdh', 'cr_fdh'))}
\arguments{
\item{data}{dataframe with columns of the variables}
\item{x}{collection of the columns with the independent variables}
\item{y}{index or name of the column with the dependent variable}
\item{ceilings}{vector with the ceiling techniques to include in this analysis}
}
\value{Returns a list with 3 items (see examples for further explanation):
\item{plots}{A list of plot-data for each x-y combination}
\item{summaries}{A list of dataframes with the summaries for each x-y combination}
\item{bottlenecks}{A list of dataframes with a bottleneck table for each ceiling technique}
}
\examples{
# Load the data
data(nca.example)
data <- nca.example

# Basic NCA analysis
# Independent variables in the first 2 columns, dependent variable in the third column
# This shows scatter plot(s) with the ceiling lines and the effect size(s) on the console
nca(data, c(1, 2), 3)

# Columns can be selected by name as well
nca(data, c('Individualism', 'Risk taking'), 'Innovation performance')

# Define the ceiling techniques via the ceilings parameter
nca(data, c(1, 2), 3, ceilings=c('ols', 'ce_vrs'))
# These are the available ceiling techniques
print(ceilings)
}
\keyword{functions}
