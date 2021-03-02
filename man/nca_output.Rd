\name{nca_output}
\alias{nca_output}
\title{display the result of the NCA analysis}
\description{Show the plots, NCA summaries and bottleneck tables of a NCA analysis.}
\usage{nca_output(model, plots=FALSE, plotly=FALSE, bottlenecks=FALSE,
           summaries=TRUE, test=FALSE, pdf=FALSE, path=NULL, selection = NULL)}
\arguments{
\item{model}{Displays output of the nca or nca_analysis command}
\item{plots}{If true shows the scatter plot(s), one  for each independent variable.\cr
In Rstudio the plots are shown the Plots window.}
\item{plotly}{If true shows the interactive scatter plot(s), one for each independent variable.\cr
In RStudio the plots are shown in the Viewer window. See Details.}
\item{bottlenecks}{If true displays the bottleneck table(s) in the Console window, one table for each ceiling line}
\item{summaries}{If true shows the summaries for each independent variable in the Console window, see Details}
\item{test}{If true shows the result of the statistical sigificance test (if present), see Details}
\item{pdf}{If true exports the output to a pdf file, except for the plotly plot}
\item{path}{Optional path for the output file(s)}
\item{selection}{Optionally selects the independent variables for inclusion in the output.\cr
For example, only the plots for the selected independent variable are shown.\cr
In the bottleneck table the required levels of only the selected independent variables for given levels of Y are shown.}
}
\details{
\strong{Plotly}\cr
The plot highlights the points that construct the ceiling line ('peers').
The names and XY coordinates of all points pop up when moving the pointer over the plot.
The toolbar allows several actions such as zoom and selection of parts of the plot. Optionally subgroups of points can be labeled.
\cr
\cr
\bold{Summaries}\cr
The output starts with 6 lines of basic information ("global") about the dataset ("Number of observations", "Scope", "Xmin", "Xmax", "Ymin", and "Ymax").
"Scope" refers to the empirical area of possible x-y combinations, given the minimum and maximum observed x and y values.\cr
The next 11 lines present the NCA parameters ("param", see below) for each of the selected ceiling techniques (the defaults techniques are ce_fdh and cr_fdh).\cr
\cr
The 11 printed NCA parameters are:\cr
- \emph{Ceiling zone}, which is the size of the "empty" area in the upper-left corner\cr
- \emph{Effect size}, which is the ceiling zone divided by the scope\cr
- \emph{# above}, which is the number of observations that are above the ceiling line and hence in the "empty" ceiling zone\cr
- \emph{c-accuracy}, which is the number of observations on or below the ceiling line divided by the total number of observations and multiplied by 100 percent\cr
- \emph{Fit}, which relates to the "closeness" of the selected ceiling line to the ce_fdh ceiling line\cr
- \emph{Slope} and \emph{Intercept}, which are the slope and the intercept of the straight ceiling line (no values are printed if the ceiling line is not a straight line, but a step function)\cr
- \emph{Abs. ineff.}, which is the total xy-space where x does not constrain y, and y is not constrained by x\cr
- \emph{Rel. ineff.}, which is the total xy-space where x does not constrain y, and y is not constrained by x as percentage of the scope\cr
- \emph{Condition ineff.}, which is the condition inefficiency that indicates for which range of x (as a percentage of the total range) x does not constrain y (i.e., there is no ceiling line in that x-range)\cr
- \emph{Outcome ineff.}, which is the outcome efficiency that indicates for which range of y (as a percentage of the total range of y) y is not constrained by x (i.e., there is no ceiling line in that y-range)
\cr
\cr
\bold{Test}\cr
NCA's statistical test is a randomness test to evaluate if the observed effect size may be a random result of unrelated X and Y variables
}
\examples{
# Use the result of the nca command:
data(nca.example)
data <- nca.example
model <- nca_analysis(data, c(1, 2), 3)

# Show the full summaries in the Console window
nca_output(model)

# Suppress the summaries and display the plots
nca_output(model, plots=TRUE, summaries=FALSE)

# Display the plots via Plotly
\donttest{nca_output(model, plotly=TRUE, summaries=FALSE)}

# Label the observation of the Plotly plot by using a vector of names (no more than 5).
# For example label the observations in nca.example
labels <- c('Australia', 'Europe', 'Europe', 'North America', 'Europe', 'Europe',
'Europe', 'Europe', 'Europe', 'Europe', 'Europe', 'Europe', 'Europe', 'Asia',
'North America', 'Europe', 'Australia', 'Europe', 'Europe', 'Europe', 'Europe',
'Asia', 'Europe', 'Europe', 'Europe', 'Europe', 'Europe', 'North America')
nca_output(model, plotly=labels, summaries=FALSE)

# Suppress the summaries and display the bottlenecks
nca_output(model, bottlenecks=TRUE, summaries=FALSE)

# Show the results of the statistical significance test (p-value)
# Make sure to set test.rep in nca_analysis
nca_output(model, test=TRUE)

# Show all five
\donttest{nca_output(model, plots=TRUE, plotly=TRUE, bottlenecks=TRUE, test=TRUE)}

# Per independent variable, export plots and summaries to PDF files,
# and export all the bottleneck tables to a single PDF file
\donttest{nca_output(model, plots=TRUE, bottlenecks=TRUE, pdf=TRUE)}
\dontshow{file.remove(dir(path=".", pattern = "*.pdf"))}
# Use the path option to export to an existing directory
outdir <- '/tmp' \dontshow{outdir <- tempdir()}
nca_output(model, plots=TRUE, pdf=TRUE, path=outdir)

# Limit the output to a selection of independent variables by name
nca_output(model, plots=TRUE, selection=c("Individualism"))

# Or by column index, in both cases the order matters
nca_output(model, plots=TRUE, selection=c(2, 1))
}
\keyword{functions}

