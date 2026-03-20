.onAttach <- function(...) {

    meta  <- packageDescription("NCA")
    year  <- sub("-.*", "", meta$Date)
    title <- meta$Title

    msg1  <- sprintf("Dul, J. & Buijs, G. %s.", year)
    msg2  <- sprintf("%s.", title)
    msg3  <- sprintf("R Package Version %s.\n", meta$Version)
    msg4  <- "URL: https://cran.r-project.org/web/packages/NCA/"

    msg5  <- "This package is based on:"
    msg6  <- "Dul, J. (2026). Necessary Condition Analysis - NCA."
    msg7  <- "Principles and Application. Chapman & Hall/CRC Press."
    msg8  <- "An online version is here:"
    msg9  <- "https://jandul.github.io/NCA/"
    
    msg10 <- "For general information about NCA see:"
    msg11 <- "https://www.erim.nl/nca"

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
                          strwrap(msg9, indent = 4, exdent = 2), "\n",
                          "\n",
                          strwrap(msg10, indent = 0, exdent = 2), "\n",
                          strwrap(msg11, indent = 2, exdent = 2), "\n")
}
