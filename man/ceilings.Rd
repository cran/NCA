\name{ceilings}
\alias{ceilings}
\docType{data}
\title{a set of all available ceiling techniques}
\description{Ceilings to use for the \code{\link{nca}} or \code{\link{nca_analysis}} methods \cr
> nca(data, c(1, 2), 3, ceilings=c('ols', 'ce_fdh', 'cr_fdh'))

\tabular{ll}{
\cr\cr\cr\cr\cr\cr\cr
\bold{Ceiling Technique} \tab \bold{Name}\cr

\bold{lh}           \tab Low-High                                         \cr
\bold{cols}         \tab Corrected Ordinary Least Squares                 \cr
\bold{qr}           \tab Quantile Regression                              \cr
\bold{sfa}          \tab Stochastic Frontier Analysis                     \cr
\bold{ce_vrs}       \tab Ceiling Envelopment with Varying Return to Scale \cr
\bold{cr_vrs}       \tab Ceiling Regression with Varying Return to Scale  \cr
\bold{ce_fdh}       \tab Ceiling Envelopment with Free Disposal Hull      \cr
\bold{cr_fdh}       \tab Ceiling Regression with Free Disposal Hull       \cr
}
}

\keyword{parameter}
