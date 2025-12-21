p_display_bottleneck <-
  function (bottlenecks, title = "Bottleneck", pdf = FALSE, path = NULL) {
    if (pdf) {
      # Put all tables in 1 file
      p_new_pdf("bottlenecks", colnames(bottlenecks[[1]])[1], path, paper = "A4r")

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

    table_matrix <- p_get_table(bn, bn.x)
    if (length(table_matrix) == 0) {
      return()
    }

    # Set y precision, names
    digits <- p_digits(bn, bn.y.id)
    col.names <- as.character(1:x.length)
    row.names <- sapply(bn[, 1], p_pretty_number, "", digits)

    if (!is.null(title) && title != "") {
      title <- paste(title, method, ":", names[1])
    }
    legend <- paste0(bn.y, " / ", bn.x, "\n")
    for (i in seq(x.length)) {
      legend <- paste0(legend, i, " ", names[i + 1], "\n")
    }

    start <- 1
    while (start < (rows)) {
      end <- min(start + 30, rows)

      part <- matrix(table_matrix[start:end,], ncol = x.length)
      colnames(part) <- col.names
      rownames(part) <- row.names[start:end]

      # A new window for each part
      textplot(part, cex = 1, halign = "left", valign = "top", mar = c(0, 0, 3, 0))
      title(title, cex.main = 1, sub = legend)

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

    table_matrix <- p_get_table(bn, bn.x)
    if (length(table_matrix) == 0) {
      return()
    }

    # Set y precision, names
    digits <- p_digits(bn, bn.y.id)
    colnames(table_matrix) <- as.character(1:x.length)
    rownames(table_matrix) <- sapply(bn[, 1], p_pretty_number, "", digits, TRUE)

    # Display header
    fmt <- sprintf(" %%-%ds", max(nchar(names)))
    cat("\n", strrep('-', dash.count), "\n", sep = "")
    message(title, " ", method, " (cutoff = ", cutoff, ")")
    message("Y", sprintf(fmt, names[1]), " (", bn.y, ")")
    for (i in seq(x.length)) {
      message(i, sprintf(fmt, names[i + 1]), " (", bn.x, ")")
    }
    cat(strrep('-', dash.count), "\n", sep = "")

    # Display table, insert the 'Y' for the first column
    cat("Y")
    print(table_matrix, quote = FALSE)
    message()
  }

p_digits <-
  function (bn, bn.y.id) {
    if (bn.y.id %in% c(1, 2)) {
      return(ifelse((100 / (nrow(bn) - 1)) %% 1 == 0, 0, 1))
    } else {
      return(p_get_digits(bn[, 1]))
    }
  }

p_get_table <-
  function (bn, bn.x) {
    rows <- nrow(bn)
    x.length <- ncol(bn) - 1

    # TODO Weird bug : can't replace colnames in bn
    table_matrix <- matrix(nrow = rows, ncol = x.length)
    for (i in 1:x.length) {
      values <- bn[, i + 1]
      if (bn.x == 'percentile') {
        cases <- attr(values, "cases")
        table_matrix[, i] <- paste0(values, ' (', as.character(cases), ')')
      } else {
        table_matrix[, i] <- values
      }
    }

    return(table_matrix)
  }