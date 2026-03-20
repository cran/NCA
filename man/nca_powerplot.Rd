\name{nca_powerplot}
\alias{nca_powerplot}
\title{Function to show the powerplot}
\description{Function to show the powerplot, optional over various variables.}
\usage{nca_powerplot(df_power, x.variable = 'n', x.name = 'Sample size',
            x.min = 0, x.max = NULL,
            line.variable = 'ES', line.name = 'Effect size')}
\arguments{
\item{df_power}{Dataframe as returned by the nca_power function.}
\item{x.variable}{The variable to use for the X scale, default is sample size.}
\item{x.name}{The name for the X scale.}
\item{x.min}{The minimum value for the X scale, defaults to 0.}
\item{x.max}{The maximum value for the X scale, defaults to the maximum X.}
\item{line.variable}{Variable that represented the line.}
\item{line.name}{The name of the variable that represented the line.}
}
\examples{
# Simple example, showing a normal plot with the power as a function of the sample size
# A line for each effect size will be shown
\dontrun{results <- nca_power(effect = c(.1, .2, .3))}
\dontshow{results <- nca_power(effect = c(.1, .2, .3), rep=2, test.rep = 1)}
\donttest{nca_powerplot(results)}

# This example shows the power for different ceiling lines
\dontrun{results <- nca_power(ceiling = c("ce_fdh", "cr_fdh"))}
\dontshow{results <- nca_power(ceiling = c("ce_fdh", "cr_fdh"), rep=2, test.rep = 1)}
\donttest{powerplot <- nca_powerplot(results, line.variable='ceiling', line.name='Ceiling')}
}
\keyword{functions}
