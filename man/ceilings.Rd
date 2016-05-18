\name{ceilings}
\alias{ceilings}
\docType{data}
\title{a set of all available ceiling techniques}
\description{Ceilings to use for the \code{\link{nca}} or \code{\link{nca_analysis}} methods \cr
  > nca(data, c(1, 2), 3, ceilings=c("ols", "ce_fdh", "cr_fdh"))

  \tabular{llll}{
  \cr\cr\cr
  \bold{Ceiling Technique} \tab \bold{Name} \tab \bold{Descriptive statistics and inferential statistics :}\cr
  \tab \tab \bold{point estimates} \tab \cr

  \bold{lh}           \tab Low-High                         \tab Yes  \tab      \cr
  \bold{cols}         \tab Corrected Ordinary Least Squares \tab Yes  \tab      \cr
  \bold{qr}           \tab Quantile Regression              \tab Yes  \tab      \cr
  \bold{sfa}          \tab Stochastic Frontier Analysis     \tab Yes  \tab      \cr
  \bold{ce_vrs}       \tab Ceiling Envelopment              \tab Yes  \tab      \cr
                      \tab with Varying Return to Scale     \tab      \tab      \cr
  \bold{cr_vrs}       \tab Ceiling Regression               \tab Yes  \tab      \cr
                      \tab with Varying Return to Scale     \tab      \tab      \cr
  \bold{ce_fdh}       \tab Ceiling Envelopment              \tab Yes  \tab      \cr
	                    \tab with Free Disposal Hull          \tab      \tab      \cr
	\bold{cr_fdh}       \tab Ceiling Regression               \tab Yes  \tab      \cr
	                    \tab with Free Disposal Hull          \tab      \tab      \cr
  }
}

\keyword{parameter}
