.onAttach <- function(...) {

    meta  <- packageDescription("NCA")
    year  <- sub("-.*", "", meta$Date)
    title <- meta$Title

    msg1  <- sprintf("Dul, J. %s.", year)
    msg2  <- sprintf("%s.", title)
    msg3  <- sprintf("R Package Version %s.\n", meta$Version)
    msg4  <- "URL: http://cran.r-project.org/web/packages/NCA/"

    msg5  <- "This package is based on:"
    msg6  <- "Dul, J. (2016) \"Necessary Condition Analysis (NCA):"
    msg7  <- "Logic and Methodology of 'Necessary but Not Sufficient' Causality.'"
    msg8  <- "Organizational Research Methods 19(1), 10-52"
    msg9  <- "http://orm.sagepub.com/content/19/1/10"

    msg10  <- "A BibTeX entry is provided by:"
    msg11  <- "citation('NCA')"

    msg12  <- "A quick start guide can be found here:"
    msg13  <- "http://repub.eur.nl/pub/78323/"
    msg14 <- "or"
    msg15 <- "https://ssrn.com/abstract=2624981"

    msg16 <- "For general information about NCA see :"
    msg17 <- "http://www.erim.nl/nca"

    packageStartupMessage("\nPlease cite the NCA package as:\n\n",
                          strwrap(msg1, indent = 2, exdent = 2), "\n",
                          strwrap(msg2, indent = 2, exdent = 2), "\n",
                          strwrap(msg3, indent = 2, exdent = 2), "\n",
                          strwrap(msg4, indent = 2, exdent = 2), "\n",
                          "\n",
                          strwrap(msg5, indent = 0, exdent = 2), "\n",
                          strwrap(msg6, indent = 2, exdent = 2), "\n",
                          strwrap(msg7, indent = 2, exdent = 2), "\n",
                          strwrap(msg8, indent = 2, exdent = 2), "\n",
                          strwrap(msg9, indent = 2, exdent = 2), "\n",
                          "\n",
                          strwrap(msg10, indent = 0, exdent = 2), "\n",
                          strwrap(msg11, indent = 2, exdent = 2), "\n",
                          "\n",
                          strwrap(msg12, indent = 0, exdent = 2), "\n",
                          strwrap(msg13, indent = 2, exdent = 2), "\n",
                          strwrap(msg14, indent = 2, exdent = 2), "\n",
                          strwrap(msg15, indent = 2, exdent = 2), "\n",
                          "\n",
                          strwrap(msg16, indent = 0, exdent = 2), "\n",
                          strwrap(msg17, indent = 2, exdent = 2), "\n")
}

