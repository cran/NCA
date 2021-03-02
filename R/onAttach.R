.onAttach <- function(...) {

    meta  <- packageDescription("NCA")
    year  <- sub("-.*", "", meta$Date)
    title <- meta$Title

    msg1  <- sprintf("Dul, J. %s.", year)
    msg2  <- sprintf("%s.", title)
    msg3  <- sprintf("R Package Version %s.\n", meta$Version)
    msg4  <- "URL: https://cran.r-project.org/web/packages/NCA/"

    msg5  <- "This package is based on:"
    msg6  <- "Dul, J. (2016) \"Necessary Condition Analysis (NCA):"
    msg7  <- "Logic and Methodology of 'Necessary but Not Sufficient' Causality.\""
    msg8  <- "Organizational Research Methods 19(1), 10-52."
    msg9  <- "https://journals.sagepub.com/doi/full/10.1177/1094428115584005"
    
    msg10 <- 'Dul, J. (2020) "Conducting Necessary Condition Analysis"'
    msg11 <- 'SAGE Publications, ISBN: 9781526460141'
    msg12 <- "https://uk.sagepub.com/en-gb/eur/conducting-necessary-condition-"
    msg13 <- "analysis-for-business-and-management-students/book262898"

    msg14 <- "Dul, J., van der Laan, E., & Kuik, R. (2020)."
    msg15 <- "A statistical significance test for Necessary Condition Analysis.\""
    msg16 <- "Organizational Research Methods, 23(2), 385-395."
    msg17 <- "https://journals.sagepub.com/doi/10.1177/1094428118795272"
    
    msg18 <- "A BibTeX entry is provided by:"
    msg19 <- "citation('NCA')"

    msg20 <- "A quick start guide can be found here:"
    msg21 <- "https://repub.eur.nl/pub/78323/"
    msg22 <- "or"
    msg23 <- "https://papers.ssrn.com/sol3/papers.cfm?abstract_id=2624981"

    msg24 <- "For general information about NCA see :"
    msg25 <- "https://www.erim.nl/nca"

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
                          "and\n",
                          strwrap(msg10, indent = 2, exdent = 2), "\n",
                          strwrap(msg11, indent = 2, exdent = 2), "\n",
                          strwrap(msg12, indent = 2, exdent = 2), "\n",
                          strwrap(msg13, indent = 2, exdent = 2), "\n",
                          "and\n",
                          strwrap(msg14, indent = 2, exdent = 2), "\n",
                          strwrap(msg15, indent = 2, exdent = 2), "\n",
                          strwrap(msg16, indent = 2, exdent = 2), "\n",
                          strwrap(msg17, indent = 2, exdent = 2), "\n",
                          "\n",
                          strwrap(msg18, indent = 0, exdent = 2), "\n",
                          strwrap(msg19, indent = 2, exdent = 2), "\n",
                          "\n",
                          strwrap(msg20, indent = 0, exdent = 2), "\n",
                          strwrap(msg21, indent = 2, exdent = 2), "\n",
                          strwrap(msg22, indent = 2, exdent = 2), "\n",
                          strwrap(msg23, indent = 2, exdent = 2), "\n",
                          "\n",
                          strwrap(msg24, indent = 0, exdent = 2), "\n",
                          strwrap(msg25, indent = 2, exdent = 2), "\n")
}

