\name{line.types}
\alias{line.types}
\docType{data}
\title{a set defining the line types for the plots}
\description{Set before calling nca_output\cr
> line.types['ce_fdh'] <- 1\cr\cr
Reset one line type by setting it to NULL\cr
> line.types['ce_fdh'] <- NULL\cr\cr
Reset all line types by setting line.types to NULL\cr
> line.types <- NULL
}
\format{
This is a list with default line types for each ceiling technique
\tabular{lllll}{
\tab \bold{ols}         \tab 1\tab \bold{lh}          \tab 5\cr
\tab \bold{cols}        \tab 3\tab \bold{qr}          \tab 4\cr
\tab \bold{ce_vrs}      \tab 5\tab \bold{cr_vrs}      \tab 1\cr
\tab \bold{ce_fdh}      \tab 6\tab \bold{cr_fdh}      \tab 1\cr
\tab \bold{sfa}         \tab 7\tab \bold{c_lp}        \tab 2\cr
}
}
\keyword{parameter}
