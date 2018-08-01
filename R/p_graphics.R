p_new_pdf <- 
function (name1, name2, path=NULL, paper="a4") {
  if (!is.null(path)) {
    name1 <- paste(path, name1, sep="/")
    gsub("//", "/", name1)
  }
  file.name <- paste(name1, gsub(" ", "_", name2), "pdf", sep=".")
  file.name <- gsub("_-_", "-", file.name)

  if (paper == "A4r") {
    pdf(file.name, paper=paper, width = 0, height = 0)
  } else {
    pdf(file.name, paper=paper)
  } 
}

p_new_window <-
function (title="", width=7, height=7) {
  # RStudio can't handle multiple windows (yet?)
  cmds <- commandArgs(trailingOnly=FALSE)
  cmd <- cmds[1]
  if (substr(tolower(cmd), nchar(cmd)-6, nchar(cmd)) == "rstudio" ||
      substr(tolower(cmd), nchar(cmd)-10, nchar(cmd)) == "rstudio.exe") {
    return ()
  }

  # Don't open windows when on Jupyter Notebooks
  if (any(grepl("kernel", ignore.case=TRUE, cmds))) {
    return()
  }

  # Don't make windows smaller than 7x7
  width <- max(7, width)
  height <- max(7, height)
  dev.new(title=title, width=width, height=height, rescale='fixed')
}