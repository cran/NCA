\name{line.colors}
\alias{line.colors}
\docType{data}
\title{a set defining the line colors for the plots}
\description{Set before calling nca_output\cr
> line.colors['ce_fdh'] <- 'blue'\cr\cr
Reset one line color by setting it to NULL\cr
> line.colors['ce_fdh'] <- NULL\cr\cr
Reset all line colors by setting line.colors to NULL\cr
> line.colors <- NULL
}
\format{
This is a list with default line colors for each ceiling technique
\tabular{lllll}{
\tab \bold{ols}         \tab 'green'          \tab \bold{lh}          \tab 'red3'\cr
\tab \bold{cols}        \tab 'darkgreen'      \tab \bold{qr}          \tab 'lightpink'\cr
\tab \bold{ce_vrs}      \tab 'orchid4'        \tab \bold{cr_vrs}      \tab 'violet'\cr
\tab \bold{ce_fdh}      \tab 'red'            \tab \bold{cr_fdh}      \tab 'orange'\cr
\tab \bold{sfa}         \tab 'darkgoldenrod'  \tab \bold{c_lp}        \tab 'blue'\cr
}
}

\keyword{parameter}
