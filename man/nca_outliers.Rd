\name{nca_outliers}
\alias{nca_outliers}
\title{Outlier detection}
\description{Detect outliers on the dataset.}
\usage{nca_outliers(data, x, y, ceiling = NULL, plotly=FALSE)}
\arguments{
\item{data}{Dataframe with columns of the variables}
\item{x}{Index or name of the column with the independent variable}
\item{y}{Index or name of the column with the dependent variable}
\item{ceiling}{Name of the ceiling technique to be used.
If not provided, both default ceilings (CE_FDH and CR_FDH) will be used}
\item{plotly}{If true shows the interactive scatter plot(s), one for each independent variable.\cr
In RStudio the plots are shown in the Viewer window. See Details.}
}
\details{
\strong{Outliers}\cr
The potential outliers are displayed with the original effects size and the
effect size if this outlier is removed. The absolute and relative differences
between both effect sizes is also shown.
The table also displays if a point is a ceiling zone outlier or a scope outlier.

\strong{Plotly}\cr
The plot highlights the potential outliers.
The names, relative difference and XY coordinates of all points pop up when moving the pointer over the plot.
The toolbar allows several actions such as zoom and selection of parts of the plot.
\cr
\cr
}
\examples{
# A basic example of the nca_outliers command:
data(nca.example)
data <- nca.example
outliers <- nca_outliers(data, 1, 3)

# This prints the outlier table
print(outliers)

# Plotly displays a scatterplot with the outliers
nca_outliers(data, 1, 3, plotly = TRUE)
}
\keyword{functions}

