nca_outliers <-
  function (data, x, y, ceiling = NULL, plotly = FALSE) {
    input.ok <- p_check_input(x, y, ceiling)
    if (isFALSE(input.ok)) {
      return()
    }
    if (is.null(input.ok)) {
      ceiling <- c("ce_fdh", "cr_fdh")
    }

    model <- nca_analysis(data, x, y, ceilings = ceiling)
    peers <- as.data.frame(model$peers)
    eff.or <- model$summaries[[1]]$params[2]
    global <- model$summaries[[1]]$global

    min.dif <- 1e-2

    outliers <- data.frame()
    for (name in rownames(data)) {
      data.new <- data[which(row.names(data) != name),]
      model.new <- nca_analysis(data.new, x, y, ceilings = ceiling)
      eff.nw <- model.new$summaries[[1]]$params[2]
      dif.abs <- eff.nw - eff.or
      dif.rel <- 100 * dif.abs / eff.or

      if (round(abs(dif.rel), digits = 2) < min.dif) {
        next
      }

      scope <- ifelse(data[name, x] %in% global[c(3, 4)]
                        || data[name, y] %in% global[c(5, 6)], "X", "")

      # Extra check: 'ceiling' outliers must be on COLS and C_LP lines
      if (ceiling[1] %in% c("cols", "c_lp") && scope == "") {
        line <- model$plots[[1]]$lines[[1]]
        if (ceiling == "cols") {
          line <- unname(coef(line))
        }
        intercept <- line[1]
        slope <- line[2]
        x.value <- data[name, x]
        y.value <- data[name, y]
        y.calc <- intercept + x.value * slope

        # If not on line, ignore ceiling zone outlier
        if (all.equal(y.value, y.calc) != TRUE) {
          next
        }
      }

      zone <- ifelse(scope == "", "X", "")
      outliers <- rbind(outliers, list(
        outliers = name, eff.or = eff.or, eff.nw = eff.nw,
        dif.abs = dif.abs, dif.rel = dif.rel, ceiling = zone, scope = scope))
    }

    outliers[, c(2, 3, 4)] <- round(outliers[, c(2, 3, 4)], digits = 2)
    outliers[, 5] <- round(outliers[, 5], digits = 1)

    if (plotly) {
      points <- data[outliers[, 1], c(x, y)]
      labels <- paste0(rownames(points), '<br>diff ', outliers[, 5])
      marks <- paste0(outliers[, 6], outliers[, 7])
      points <- cbind(points, labels = labels, marks)
      points <- points[points[, 4] != '', 1:3]
      p_display_plotly(model$plots[[1]], points, NULL, name = 'outlier')
    }

    return(outliers)
  }

p_check_input <-
  function (x, y, ceiling) {
    if (length(x) != 1) {
      message()
      message("Outlier detection needs a single independent variable")
      return(FALSE)
    }
    if (length(y) != 1) {
      message()
      message("Outlier detection needs a single dependent variable")
      return(FALSE)
    }
    if (is.null(ceiling)) {
      return(NULL)
    }
    if (length(ceiling) != 1) {
      message()
      message("Outlier detection needs a single ceiling")
      return(FALSE)
    }
    if (ceiling == "ols") {
      message()
      message("Outlier detection does not work with OLS")
      return(FALSE)
    }

    return(TRUE)
  }
