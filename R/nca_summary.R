p_display_summary <-
function (summary, pdf=FALSE, path=NULL) {
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

  p_new_pdf("summary", p_generate_title(x.name, y.name), path, paper="A4r")

  # Set page layout
  par(mfrow=c(3, 1))
  par(family="mono")

  # Plot the 2 dataframes
  textplot(p_pretty_global(summary$global),
           cex=1.45, halign="left", mar=c(0,0,4,0), rmar=1)
  title <- paste("NCA Parameters :", p_generate_title(x.name, y.name))
  par(family="")
  title(title, cex.main=1.45)
  par(family="mono")

  if (ncol(summary$params) == 0) {
    textplot(" No NCA parameters available because only OLS selected\n", cex=1.45, halign="left")
  } else {
    textplot(p_pretty_params(summary$params),
             cex=1.45, halign="left", mar=c(0,0,0,0), rmar=1)
  }

  dev.off()
  cat("")
}

p_display_summary_screen <-
function (summary) {
  x.name <- summary$names[1]
  y.name <- summary$names[2]
  title <- paste("NCA Parameters :", p_generate_title(x.name, y.name))

  cat("\n----------------------------------------")
  cat("----------------------------------------\n")
  message(title)
  cat("----------------------------------------")
  cat("----------------------------------------\n")
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
    warning("\n No effect sizes available because only OLS selected\n", call.=FALSE)
    return()
  }

  simple <- matrix(nrow=length(names(summaries)),
                   ncol=2 * length(colnames(summaries[[1]]$params)))

  for (i in 1:length(names(summaries))) {
    x.name <- names(summaries)[i]
    tmp <- summaries[[x.name]]$params
    for (j in 1:length(colnames(tmp))) {
      simple[i, 2*j - 1] <- ifelse(is.nan(tmp[2,j]), NA, sprintf("%.2f", tmp[2,j]))
      simple[i, 2*j] <- ifelse(is.nan(tmp[6,j]), NA, sprintf("%.3f", tmp[6,j]))
    }
  }

  colnames <- colnames(summaries[[1]]$params)
  colnames(simple) <- c(rbind(colnames, rep("p", length(colnames))))
  rownames(simple) <- names(summaries)

  # Remove columns if no tests are present
  simple[simple == "NA"] <- NA
  colnames(simple)[colSums(!is.na(simple)) == 0] <- ''

  cat("\n----------------------------------------")
  cat("----------------------------------------\n")
  message("Effect size(s):")
  print(simple, na.print="", quote=FALSE)
  cat("----------------------------------------")
  cat("----------------------------------------\n\n")
}

p_summary <-
function (analyses, loop.data) {
  # Do the 'global statistics'
  obs <- min(length(loop.data$x), length(loop.data$y))
  emp <- loop.data$scope.emp
  scope.emp.area <- (emp[2] - emp[1]) * (emp[4] - emp[3])

  if (identical(loop.data$scope.theo, loop.data$scope.emp)) {
    mat1 <- matrix(ncol=1, nrow=6)
    mat1[1,1] <- obs
    mat1[2,1] <- loop.data$scope.area
    mat1[3:6] <- loop.data$scope.emp
    colnames(mat1) <- c("")
    rownames(mat1) <- p_GLOBAL_NAMES
  } else {
    mat1 <- matrix(ncol=2, nrow=6)
    mat1[1,] <- c(obs, NA)
    mat1[2,] <- c(scope.emp.area, loop.data$scope.area)
    mat1[3:6, 1] <- loop.data$scope.emp
    mat1[3:6, 2] <- loop.data$scope.theo
    colnames(mat1) <- c("", " ")
    rownames(mat1) <- replace(p_GLOBAL_NAMES, 2, "Scope  emp / theo")
  }

  # And the parameter statistics
  data2 <- c(sapply(analyses, function(x) x$ceiling),
             sapply(analyses, function(x) x$effect),
             sapply(analyses, function(x) x$above),
             sapply(analyses, function(x) x$accuracy),
             sapply(analyses, function(x) x$fit),
             sapply(analyses, function(x) x$p),
             sapply(analyses, function(x) x$p_accuracy),
             sapply(analyses, function(x) NA),
             sapply(analyses, function(x) x$slope),
             sapply(analyses, function(x) x$intercept),
             sapply(analyses, function(x) x$ineffs$abs),
             sapply(analyses, function(x) x$ineffs$rel),
             sapply(analyses, function(x) x$ineffs$x),
             sapply(analyses, function(x) x$ineffs$y))
  mat2 <- t(matrix(data2[names(data2) != "ols"], ncol=14))
  colnames(mat2) <- names(analyses)[names(analyses) != "ols"]
  rownames(mat2) <- p_RESULT_NAMES

  names <- c(colnames(loop.data$x), colnames(loop.data$y))

  return ( list(global=mat1, params=mat2, names=names) )
}

p_pretty_global <-
function (global) {
  digits <- p_get_digits(global[3:6, ])

  pretty <- matrix(ncol=ncol(global), nrow=6)
  for (row in 1:6) {
    pretty[row, 1] <- ifelse(row == 1,
      p_pretty_number(global[row, 1], " ", prec=0, useSpaces=TRUE),
      p_pretty_number(global[row, 1], "", digits, TRUE))
    if (ncol(global) > 1) {
      pretty[row, 2] <- ifelse(row == 1, "",
        p_pretty_number(global[row, 2], "", digits, TRUE))
    }
  }

  colnames(pretty) <- rep(" ", ncol(pretty))
  rownames(pretty) <- rownames(global)

  return( as.data.frame(pretty) )
}

p_pretty_params <-
function (params) {
  pretty <- data.frame(params)

  for (row in 1:nrow(params)) {
    for (col in 1:ncol(params)) {
      if (row == 3) {
        pretty[row, col] <- p_pretty_number(params[row, col], prec=0, useSpaces=TRUE)
      } else if (row == 4 || row == 5) {
        s <- params[row, col]
        if (!p_is_number(s)) {
          pretty[row, col] <- ifelse(row == 5 && is.na(s), "NA    ", "")
        } else if (s %% 1 == 0) {
          s <- p_pretty_number(s, "", 0)
          pretty[row, col] <- sprintf("%s%%   ", s)
        } else {
          s <- p_pretty_number(s, "", 1)
          pretty[row, col] <- sprintf("%s%% ", s)
        }
      } else {
        pretty[row, col] <- p_pretty_number(params[row, col])
      }
    }
  }

  if (all(pretty[7, ] == "")) {
    pretty <- pretty[-c(6, 7), , drop=FALSE]
  }

  return( pretty )
}