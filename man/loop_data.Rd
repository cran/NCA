\name{loop_data}
\alias{loop_data}
\title{Create list with relevant data for x and y index}
\description{Create list with relevant data for x and y index}
\usage{
  loop_data(data, id.x, id.y, nx)
}
\arguments{
  \item{data}{Data object on which to run a analysis}
  \item{id.x}{Index of independent variable}
  \item{id.y}{Index of dependent variable}
  \item{nx}{Total number of independent variables}
}

\value{Retuns the relevant variables for this independent/dependent pair:\cr
  \item{id.x, id.y}{indices for this pair in the data}
  \item{x, y}{independent/dependent data}
  \item{scope}{potential area of all observations}
  \item{names}{names of the variables}
  \item{x.low, x.high}{min/max values of independent data}
  \item{y.low, y.high}{min/max values of dependent data}
}

\keyword{functions}
