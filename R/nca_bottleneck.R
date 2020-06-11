p_display_bottleneck <-
function (bottlenecks, title="Bottleneck", pdf=FALSE, path=NULL) {
  if (pdf) {
    # Put all tables in 1 file
    p_new_pdf("bottlenecks", colnames(bottlenecks[[1]])[1], path, paper="A4r")

    for (method in names(bottlenecks)) {
      p_display_table_pdf(bottlenecks[[method]], p_pretty_name(method), title)
    }

   # Close the file
    dev.off()
    cat("")
  } else {
    for (method in names(bottlenecks)) {
      p_display_table_screen(bottlenecks[[method]], p_pretty_name(method), title)
    }
  }
}

p_display_table_pdf <-
function (bn, method, title) {
  names <- colnames(bn)
  bn.x <- attr(bn, "bn.x")
  bn.y <- attr(bn, "bn.y")
  bn.y.id <- attr(bn, "bn.y.id")
  rows <- nrow(bn)
  x.length <- ncol(bn) - 1
  cutoff <- attr(bn, "cutoff")

  # TODO Weird bug : can't replace colnames in bn
  tmp <- matrix(nrow=rows, ncol=x.length)
  for (i in 1:x.length) {
    tmp[, i] <- bn[,i+1]
  }
  if (length(tmp) == 0) {
    return()
  }

  # Set y precision
  if (bn.y.id %in% c(1, 2)) {
    digits <- ifelse((100 / (rows-1)) %% 1 == 0, 0, 1)
  } else {
    digits <- p_get_digits(bn[,1])
  }
  col.names <- as.character(c(1:x.length))
  row.names <- sapply(bn[,1], p_pretty_number, "", digits)

  if (!is.null(title) && title != "") {
    title <- paste(title, method, ":", names[1])
  }
  legend <- paste0(bn.y, " / ", bn.x, "\n")
  for (i in seq(x.length)) {
    legend <- paste0(legend, i, " ", names[i+1], "\n")
  }

  start <- 1
  while (start < (rows)) {
    end <- min(start + 30, rows)

    tmp.part <- matrix(tmp[start:end,], ncol=x.length)
    colnames(tmp.part) <- col.names
    rownames(tmp.part) <- row.names[start:end]

    # A new window for each part
    textplot(tmp.part, cex=1, halign="left", valign="top", mar=c(0, 0, 3, 0))
    title(title, cex.main=1, sub=legend)

    start <- end
  }
}

p_display_table_screen <-
function (bn, method, title) {
  names <- colnames(bn)
  bn.x <- attr(bn, "bn.x")
  bn.y <- attr(bn, "bn.y")
  bn.y.id <- attr(bn, "bn.y.id")
  rows <- nrow(bn)
  x.length <- ncol(bn) - 1
  cutoff <- attr(bn, "cutoff")

  # TODO Weird bug : can't replace colnames in bn
  tmp <- matrix(nrow=rows, ncol=x.length)
  for (i in 1:x.length) {
    tmp[, i] <- bn[,i+1]
  }
  if (length(tmp) == 0) {
    return()
  }

  # Set y precision
  if (bn.y.id %in% c(1, 2)) {
    digits <- ifelse((100 / (rows-1)) %% 1 == 0, 0, 1)
  } else {
    digits <- p_get_digits(bn[,1])
  }
  colnames(tmp) <- as.character(c(1:x.length))
  rownames(tmp) <- sapply(bn[,1], p_pretty_number, "", digits, TRUE)

  # Display header
  fmt <- sprintf(" %%-%ds", max(nchar(names)))
  cat("\n----------------------------------------")
  cat("----------------------------------------\n")
  message(title, " ", method, " (cutoff = ", cutoff, ")")
  message("Y", sprintf(fmt, names[1]), " (", bn.y, ")")
  for (i in seq(x.length)) {
    message(i, sprintf(fmt, names[i+1]), " (", bn.x ,")")
  }
  cat("----------------------------------------")
  cat("----------------------------------------\n")

  # Display table, insert the 'Y' for the first column
  cat("Y")
  print(tmp, quote=FALSE)
  message()
}
