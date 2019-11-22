globalVariables('i')

p_test <-
function (analyses, loop.data, test.params, effect_aggregation) {
  if (test.params$rep < 1) {
    return (NULL)
  }
  test.params$rep <- round(test.params$rep)
  test.params$p_confidence <- max(0, min(1, test.params$p_confidence))
  test.params$p_threshold <- max(0, min(1, test.params$p_threshold))

  # Add statistics tests
  test <- list()
  h <- length(loop.data$x)
  y_org <- loop.data$y
  effect.sims <- list()
  # But not for OLS
  ceilings <- names(analyses)[names(analyses) != "ols"]

  # test.rep can never be larger than n!, only test for small h's
  # fact(15) is 1e12, if user chooses that for test.rep he's got other problems
  if (h < 16 && test.params$rep > factorial(h)) {
    test.params$rep <- factorial(h)
    fmt = "\nLowered test.rep to %s as it can not be larger than N!\n"
    cat(sprintf(fmt, test.params$rep))
  }

  # Ignore errors resulting from sampling
  old.warning <- getOption("warn")
  options(warn = -1)
  # Do you remember?
  start <- Sys.time()
  # Shorten the X name
  x.name <- paste0(strtrim(colnames(loop.data$x), 25),
    ifelse(nchar(colnames(loop.data$x)) > 25, "...", ""))

  # Create a unique list of samples
  samples <- foreach (i=1:test.params$rep) %dopar% {
    set.seed(i)
    sample(1:h, h, replace = FALSE)
  }

  for (ceiling in ceilings) {
    cat(paste("Do test for   :", ceiling, "-", x.name))

    effect.sims[[ceiling]] <- foreach (sample=iter(samples)) %dopar% {
      loop.data$y <- y_org[unlist(sample)]

      # We need to make sure ce_cm_conf (if present) comes before cr_cm_conf
      if ("ce_cm_conf" %in% ceilings) {
        analisys_ce_cm_conf <- p_nca_wrapper("ce_cm_conf", loop.data, NULL, effect_aggregation)
        loop.data$ce_cm_conf_columns <- attr(analisys_ce_cm_conf$line, "columns")
      }
      if (ceiling == "ce_cm_conf") {
        analysis <- analisys_ce_cm_conf
      } else {
        analysis <- p_nca_wrapper(ceiling, loop.data, NULL, effect_aggregation)
      }
      return (analysis$effect)
    }

    # Convert to a vector, remove NA
    effect.sims[[ceiling]] <- unlist(effect.sims[[ceiling]])
    effect.sims[[ceiling]] <- effect.sims[[ceiling]][!is.na(effect.sims[[ceiling]])]

    cat(paste("\rDone test for :", ceiling, "-", x.name, "\n"))
  }

  # Start caring about errors again
  options(warn = old.warning)

  for (ceiling in ceilings) {
    observed <- analyses[[ceiling]]$effect
    data <- effect.sims[[ceiling]]

    if (is.null(data)) {
      fmt <- "No permutation test for %s on %s\n"
      cat(sprintf(fmt, loop.data$names[1], ceiling))
    }

    # Add threshold- and P-value for displaying in summary
    threshold.value <- as.numeric(quantile(sort(data), 1 - test.params$p_threshold))
    p.value <- 1 - mean(data < observed)
    p.value <- max(p.value, 1/test.params$rep)

    # Add MOE
    uns <- test.params$p_confidence + 0.5 * (1 - test.params$p_confidence)
    test.params$p_accuracy <- qnorm(uns) * sqrt(p.value * (1 - p.value) / test.params$rep)

    names <- c(colnames(loop.data$x), colnames(y_org))
    test[[ceiling]] <- list(data = data, observed = observed, test.params=test.params,
                            p.value = p.value, threshold.value = threshold.value,
                            names = names)
  }

  return (list(test=test, test.time=difftime(Sys.time(), start, units="secs")))
}

p_test_time <-
function (test.time) {
  test.time <- as.numeric(test.time)
  if (test.time > 86400) {
    duration <- sprintf("%s days, %02d:%02d:%02d",
      test.time %/% 86400,
      test.time %% 86400 %/% 3600,
      test.time %% 3600 %/% 60,
      test.time %% 60 %/% 1)
  }
  else {
    duration <- sprintf("%02d:%02d:%02d",
      test.time %/% 3600,
      test.time %% 3600 %/% 60,
      test.time %% 60 %/% 1)
  }
  return (paste("Test done in", duration))
}

p_display_test <-
function (test, pdf=FALSE, path=NULL) {
  for (ceiling in names(test)) {
    p_display_ceiling_test(ceiling, test[[ceiling]], pdf, path)
  }
}

p_display_ceiling_test <-
function (ceiling, ceiling_test, pdf=FALSE, path=NULL) {
  ceiling <- p_pretty_name(ceiling)
  data <- ceiling_test$data
  observed <- ceiling_test$observed
  p.value <- ceiling_test$p.value
  threshold.value <- ceiling_test$threshold.value
  p_threshold <- ceiling_test$test.params$p_threshold
  names <- ceiling_test$names

  # All Y-values are equal
  if (is.null(data)) {
    return(NULL)
  }

  bin.count <- 30
  x.low <- - 0.05
  x.high <- max(data, observed) + 0.01

  label.main <- sprintf("X=%s  Y=%s  %s\n", names[1], names[2], ceiling)
  label.random <- ifelse(p_threshold > 0, sprintf(
      "----  random (d = %.3f, p_threshold %.2f)", threshold.value, p_threshold), "")
  label.p <- ""
  if (!is.na(p.value)) {
    p_accuracy <- ceiling_test$test.params$p_accuracy
    rep <- ceiling_test$test.params$rep
    p.min <- max(0, p.value - p_accuracy)
    p.max <- min(1, p.value + p_accuracy)
    label.p <- sprintf(", p = %.3f [%.3f, %.3f], rep = %d",
                       p.value, p.min, p.max, rep)
  }
  label.observed <- sprintf("observed (d = %.3f%s)", observed, label.p)

  # Add data plot and title
  obj <- qplot(data, bins = bin.count, geom = 'histogram',
               xlim = c(x.low, x.high), xlab = "effect size",
               fill = I("white"), col = I("black"), na.rm = TRUE)
  obj <- obj + labs(title = label.main, subtitle = "")
  obj <- obj + theme(plot.title=element_text(hjust=0.5))

  # Add lines
  if (p_threshold > 0) {
    obj <- obj + geom_vline(xintercept = threshold.value, col = "darkgreen", lwd = .5, lty = 5)
  }
  obj <- obj + geom_vline(xintercept = observed, col = "red", lwd = 0.5, lty = 1)

  # Open new window or PDF
  if (pdf) {
    title <- paste(paste(ceiling_test$names, collapse="-"), "random test")
    p_new_pdf("plot", title, path)
  } else {
    p_new_window(title = paste("Random test ", ceiling))
    par(family = "")
    par(mfrow = c(1, 1))
  }

  # Plot all
  plot(obj)

  # Add legend for lines
  p_add_legend(label.random, label.observed, p_threshold)

  if (pdf) {
    dev.off()
    cat("")
  }
}

p_add_legend <-
function (label.random, label.observed, p_threshold) {
  gp.r <- gpar(col = "darkgreen", fontsize = 11)
  gp.o <- gpar(col = "red", fontsize = 11)
  gp.o1 <- gpar(col = "red", fontsize = 10, fontface="bold")
  if (p_threshold > 0) {
    pos.x  <- unit(0.5, "npc") - unit(5, "cm")
    pos.x1 <- pos.x + unit(.75, "cm")
    pos.y  <- unit(1, "npc") - unit(2, "line")
    pos.y1 <- unit(1, "npc") - unit(3.0, "line")
    pos.y2 <- unit(1, "npc") - unit(3.0, "line")

    grid.text(label.random,   pos.x,  pos.y,  just="left", gp = gp.r)
    grid.text("___",          pos.x,  pos.y1, just="left", gp = gp.o1)
    grid.text(label.observed, pos.x1, pos.y2, just="left", gp = gp.o)
  }
  else {
    pos.x <- unit(0.5, "npc")
    pos.y <- unit(1, "npc") - unit(2, "line")
    grid.text(label.observed, pos.x, pos.y, just="centre", gp = gp.o)
  }
}
