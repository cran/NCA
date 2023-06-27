\name{nca_outliers}
\alias{nca_outliers}
\title{Outlier detection}
\description{Detect outliers on the dataset.}
\usage{nca_outliers(data, x, y, ceiling = NULL,
corner = NULL, flip.x = FALSE, flip.y = FALSE, scope=NULL,
k = 1, min.dif = 1e-2, max.results = 25, plotly=FALSE, condensed = FALSE)}
\arguments{
\item{data}{Dataframe with columns of the variables}
\item{x}{Index or name of the column with the independent variable}
\item{y}{Index or name of the column with the dependent variable}
\item{ceiling}{Name of the ceiling technique to be used.
If not provided, the default ceilings (CE_FDH) will be used}
\item{corner}{either an integer or a vector of integers, indicating the corner to analyze, see Details}
\item{flip.x}{reverse the direction of the independent variables\cr
 Use either a boolean for all independent variables, or a vector with the same length as x}
\item{flip.y}{reverse the direction of the dependent variables, boolean}
\item{scope}{a theoretical scope in list format : (x.low, x.high, y.low, y.high), see nca_analysis}
\item{k}{use combinations of observations, default is 1 (single observations)}
\item{min.dif}{set the threshold for the minimum dif.rel to be considered as outlier, default is 1e-2}
\item{max.results}{only show the first 'max.results' outliers, default is 25}
\item{plotly}{If true shows the interactive scatter plot(s), one for each independent variable.\cr
In RStudio the plots are shown in the Viewer window. See Details.}
\item{condensed}{If true and k > 1, hide outlier combinations for which the effect size is\cr
not increased compared to the effect size of single elements of that combination. (default FALSE)}
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
outliers <- nca_outliers(nca.example, 1, 3)

# This prints the outlier table
print(outliers)

# Plotly displays a scatterplot with the outliers
nca_outliers(nca.example, 1, 3, plotly = TRUE)

# Test for combinations of observations
# Useful to detect clusters of observations as possible outliers
nca_outliers(nca.example, 1, 3, k = 2)

# Just like the nca_analysis command, nca_outliers accept both flip and corner arguments
nca_outliers(nca.example, 1, 3, corner=3)

# It is possible to define the maximum number of results (default is 25)
nca_outliers(nca.example, 1, 3, max.results=5)

# Do no show possible outliers where the abs(dif.rel) is smaller than min.dif
nca_outliers(nca.example, 1, 3, min.dif=10)

# If k > 1, the effect size of a single observation might not change
# when paired with another observation, e.g. dif.rel of Obs1 == dif.rel of Obs1+Obs2.
# The example below hides combinations of Japan with Portugal, Greece, etc.
nca_outliers(nca.example, 1, 3, k = 2, condensed = TRUE)
}
\keyword{functions}
