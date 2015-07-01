\name{nca}
\alias{nca}
\title{Run NCA analyses on a data set}
\description{Run multiple types of NCA analyses on a dataset.\cr
Show the resulting graphs and tables on-screen or write to PDF.}
\usage{nca(data, nx, ny, 
          ols=TRUE, cols=FALSE, qr=FALSE, lh=FALSE, 
          ce_vrs=FALSE, cr_vrs=FALSE, ce_fdh=TRUE, cr_fdh=TRUE, sfa=FALSE, 
          title="NCA Plot", use.title=TRUE, pdf=FALSE, prefix="out",
          results=FALSE, bottleneck.x='percentage.range',
          bottleneck.y='percentage.range', steps=10, cutoff=0)}
\arguments{
  \item{data}{data object on which to run the analyses}
  \item{nx}{number of independent variables}
  \item{ny}{number of dependent variables}
  \item{ols}{include OLS in the nca analysis}
  \item{cols}{include COLS in the nca analysis}
  \item{qr}{include QR in the nca analysis}
  \item{lh}{include LH in the nca analysis}
  \item{ce_vrs}{include CE VRS in the nca analysis}
  \item{cr_vrs}{include CR VRS in the nca analysis}
  \item{ce_fdh}{include CE FDH in the nca analysis}
  \item{cr_fdh}{include CR FDH in the nca analysis}
  \item{sfa}{include SFA in the nca analysis}
  \item{title}{title of the NCA plot}
  \item{use.title}{plot the titles of graphs and tables}
  \item{pdf}{write to PDF instead of screen}
  \item{prefix}{use as prefix for PDF files}
  \item{results}{display the results table}
  \item{bottleneck.x}{options for displaying the independent variables in the bottleneck table\cr
   'percentage.range' to display the percentage of range between min(x) and max(x)\cr
   'percentage.max' to display the percentage of max(x)\cr
   'actual' to display the actual values\cr
   'percentile' to display the percentiles\cr
   Using percentage.max with negative values might yield counterintuitive results}
  \item{bottleneck.y}{options for displaying the dependent variables in the bottleneck table, \cr see bottleneck.x}
  \item{steps}{number of steps in the bottleneck table}
  \item{cutoff}{display calculated x,y values that are lower/higher than lowest/highest observed x,y values in the bottleneck table as:\cr
  0 : NA\cr 1 : lowest/highest observed values\cr 2 : calculated values}
}
\keyword{functions}
