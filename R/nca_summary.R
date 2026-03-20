p_display_summary <-
  function (summary, pdf = FALSE, path = NULL) {
    if (pdf) {
      p_display_summary_pdf(summary, path)
    } else {
      p_display_summary_screen(summary)
    }
  }

p_display_summary_pdf <-
  function (summary, path) {
    x.name <- summary$names[1]
    y.name <- summary$names[2]

    p_new_pdf("summary", p_generate_title(x.name, y.name), path, paper = "A4r")

    # Set page layout
    layout(matrix(c(1, 2), ncol = 1), heights = c(1, 3))
    par(family = "mono")

    # Plot the 2 dataframes
    textplot(p_pretty_global(summary$global),
             cex = 1, halign = "left", mar = c(0, 0, 4, 0), rmar = 1)
    title <- paste("NCA Parameters :", p_generate_title(x.name, y.name))
    par(family = "")
    title(title, cex.main = 1)
    par(family = "mono")

    if (ncol(summary$params) == 0) {
      textplot(" No NCA parameters available because only OLS selected\n",
               cex = 1, halign = "left")
    } else {
      textplot(p_pretty_params(summary$params),
               cex = 1, halign = "left", mar = c(0, 0, 0, 0), rmar = 1)
    }

    dev.off()
    cat("")
  }

p_display_summary_screen <-
  function (summary) {
    x.name <- summary$names[1]
    y.name <- summary$names[2]
    title <- paste("NCA Parameters :", p_generate_title(x.name, y.name))

    cat("\n", strrep('-', dash.count), "\n", sep = "")
    message(title)
    cat(strrep('-', dash.count), "\n", sep = "")
    print(p_pretty_global(summary$global))
    cat("\n")

    if (ncol(summary$params) == 0) {
      message(" No NCA parameters available because only OLS selected\n")
    } else {
      print(p_pretty_params(summary$params))
    }
    cat("\n")
  }

p_display_summary_simple <-
  function (summaries) {
    if (ncol(summaries[[1]]$params) == 0) {
      warning("\n No effect sizes available because only OLS selected\n", call. = FALSE)
      return()
    }

    simple <- matrix(nrow = length(names(summaries)),
                     ncol = 2 * length(colnames(summaries[[1]]$params)))

    for (i in seq_along(names(summaries))) {
      x.name <- names(summaries)[i]
      tmp <- summaries[[x.name]]$params
      for (j in seq_along(colnames(tmp))) {
        tmp.2 <- unlist(tmp[2, j])
        tmp.6 <- unlist(tmp[6, j])
        simple[i, 2 * j - 1] <- ifelse(is.nan(tmp.2), NA, sprintf("%.2f", tmp.2))
        simple[i, 2 * j] <- ifelse(is.nan(tmp.6), NA, sprintf("%.3f", tmp.6))
      }
    }

    colnames <- colnames(summaries[[1]]$params)
    colnames(simple) <- c(rbind(colnames, rep("p", length(colnames))))
    rownames(simple) <- names(summaries)

    # Remove columns if no tests are present
    simple[simple == "NA"] <- NA
    colnames(simple)[colSums(!is.na(simple)) == 0] <- ''

    cat("\n", strrep('-', dash.count), "\n", sep = "")
    message("Effect size(s):")
    print(simple, na.print = "", quote = FALSE)
    cat(strrep('-', dash.count), "\n\n", sep = "")
  }

p_summary <-
  function (analyses, loop.data) {
    # Do the 'global statistics'
    obs <- min(length(loop.data$x), length(loop.data$y))
    emp <- loop.data$scope.emp
    scope.emp.area <- (emp[2] - emp[1]) * (emp[4] - emp[3])

    if (identical(loop.data$scope.theo, loop.data$scope.emp)) {
      mat1 <- matrix(ncol = 1, nrow = 6)
      mat1[1, 1] <- obs
      mat1[2, 1] <- loop.data$scope.area
      mat1[3:6] <- loop.data$scope.emp
      colnames(mat1) <- c("")
      rownames(mat1) <- p_GLOBAL_NAMES
    } else {
      mat1 <- matrix(ncol = 2, nrow = 6)
      mat1[1,] <- c(obs, NA)
      mat1[2,] <- c(scope.emp.area, loop.data$scope.area)
      mat1[3:6, 1] <- loop.data$scope.emp
      mat1[3:6, 2] <- loop.data$scope.theo
      colnames(mat1) <- c("", " ")
      rownames(mat1) <- replace(p_GLOBAL_NAMES, 2, "Scope  emp / theo")
    }

    # And the parameter statistics (order corresponds to p_PARAM_NAMES)
    data2 <- c(sapply(analyses, function (x) x$ceiling),
               sapply(analyses, function (x) x$effect),
               sapply(analyses, function (x) x$above),
               sapply(analyses, function (x) x$slope),
               sapply(analyses, function (x) x$intercept),
               sapply(analyses, function (x) x$p),
               sapply(analyses, function (x) x$p_accuracy),
               sapply(analyses, function (x) NA),
               sapply(analyses, function (x) x$complexity),
               sapply(analyses, function (x) x$fit),
               sapply(analyses, function (x) x$accuracy),
               sapply(analyses, function (x) x$accuracy_nof),
               sapply(analyses, function (x) x$noise_pct),
               sapply(analyses, function (x) x$noise_nof),
               sapply(analyses, function (x) x$exceptions_pct),
               sapply(analyses, function (x) x$exceptions_nof),
               sapply(analyses, function (x) x$support_pct),
               sapply(analyses, function (x) x$support_nof),
               sapply(analyses, function (x) x$spread),
               sapply(analyses, function (x) x$sharpness),
               sapply(analyses, function (x) NA),
               sapply(analyses, function (x) x$ineffs$abs),
               sapply(analyses, function (x) x$ineffs$rel),
               sapply(analyses, function (x) x$ineffs$x),
               sapply(analyses, function (x) x$ineffs$y))
    mat2 <- t(matrix(data2[names(data2) != "ols"], ncol = length(p_PARAM_NAMES)))
    colnames(mat2) <- names(analyses)[names(analyses) != "ols"]
    rownames(mat2) <- p_PARAM_NAMES

    names <- c(colnames(loop.data$x), colnames(loop.data$y))

    return(list(global = mat1, params = mat2, names = names))
  }

p_pretty_global <-
  function (global) {
    digits <- p_get_digits(global[3:6,])

    pretty <- matrix(ncol = ncol(global), nrow = 6)
    for (row in 1:6) {
      pretty[row, 1] <- ifelse(row == 1,
                               p_pretty_number(global[row, 1], " ", prec = 0, useSpaces = TRUE),
                               p_pretty_number(global[row, 1], "", digits, TRUE))
      if (ncol(global) > 1) {
        pretty[row, 2] <- ifelse(row == 1, "",
                                 p_pretty_number(global[row, 2], "", digits, TRUE))
      }
    }

    colnames(pretty) <- rep(" ", ncol(pretty))
    rownames(pretty) <- rownames(global)

    return(as.data.frame(pretty))
  }

p_pretty_params <-
  function (params) {
    # Extra columns for the NOF items
    pretty <- data.frame(params)
    ids <- newnames <- NULL
    for (i in seq_len(ncol(params))) {
      ids <- c(ids, i, i)
      newnames <- c(newnames, colnames(params)[i], " ")
    }
    pretty <- pretty[, ids]
    pretty[, c(FALSE, TRUE)] <- ""
    colnames(pretty) <- newnames

    for (row in seq_len(nrow(params))) {
      rowname <- rownames(params)[row]
      for (col in seq_len(ncol(params))) {
        item <- unlist(params[row, col])
        if (rowname %in% c('Complexity', '# above')) {
          pretty[row, 2 * col - 1] <- p_pretty_number(item, prec = 0, useSpaces = TRUE)
        }
        else if (rowname == "Accuracy nof") {
          pretty["Ceiling accuracy", 2 * col] <- p_nof(item)
        }
        else if (rowname == "Noise nof") {
          pretty["Noise", 2 * col] <- p_nof(item)
        }
        else if (rowname == "Exceptions nof") {
          pretty["Exceptions", 2 * col] <- p_nof(item)
        }
        else if (rowname == "Support nof") {
          pretty["Support", 2 * col] <- p_nof(item)
        }
        else if (rowname %in% c('Ceiling accuracy', 'Fit', 'Noise', 'Exceptions', 'Support')) {
          if (!p_is_number(item)) {
            pretty[row, 2 * col - 1] <- ifelse(rowname == 'Fit' && is.na(item), "NA    ", "")
          } else if (item %% 1 == 0) {
            item <- p_pretty_number(item, "", 0)
            pretty[row, 2 * col - 1] <- sprintf("%s  %% ", item)
          } else {
            item <- p_pretty_number(item, "", 1)
            pretty[row, 2 * col - 1] <- sprintf("%s%% ", item)
          }
        }
        else if (rowname %in% c("Spread", "Sharpness")) {
          pretty[row, 2 * col - 1] <- p_pretty_number(item, prec = 2, useSpaces = TRUE)
        }
        else {
          pretty[row, 2 * col - 1] <- p_pretty_number(item)
        }
      }
    }

    drop.names <- c('Noise nof', 'Exceptions nof', 'Support nof', 'Accuracy nof')
    if (all(pretty['p-value',] == "")) {
      drop.names <- c(drop.names, 'p-value', 'p-accuracy')
    }
    pretty <- pretty[!(rownames(pretty) %in% drop.names), , drop = FALSE]

    return(pretty)
  }
