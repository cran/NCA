nca_power <-
  function (n = c(20, 50, 100), effect = 0.10, slope = 1, ceiling = "ce_fdh",
            corner = 1, p = 0.05,
            distribution.x = "uniform", distribution.y = "uniform",
            rep = 100, test.rep = 200) {

    if (any(effect <= 0) || any(effect >= 1)) {
      cat("The effect size needs to be larger than 0 and smaller than 1\n")
      return()
    }
    if (!is.null(corner)) {
      p_validate_corner(1, corner)
      if (slope > 0 && (corner %in% c(2, 3))) {
        cat(p_errors[['corner_23']], "\n")
        return(NULL)
      }
      if (slope < 0 && (corner %in% c(1, 4))) {
        cat(p_errors[['corner_14']], "\n")
        return(NULL)
      }
      if (corner != 1) {
        cat("Using corner ", corner, "\n")
      }
    }

    # Never do purity
    p_setenv(p_skip_purity)

    # Make sure we're not doing extra work
    distribution.x <- unique(distribution.x)
    distribution.y <- unique(distribution.y)
    n <- unique(n)
    effect <- unique(effect)
    slope <- unique(slope)
    ceiling <- unique(ceiling)

    # Calculate the total number of iterations
    n_batch <- (
      length(distribution.x) *
        length(distribution.y) *
        length(n) *
        length(effect) *
        length(slope) *
        length(ceiling))
    n_iterations <- rep * n_batch

    # Define the variables that will store results
    results <- data.frame(n = numeric(), ES = numeric(), slope = numeric(),
                          ceiling = character(),
                          distribution.x = character(), distribution.y = character(),
                          power = numeric())

    # Start a cluster if needed
    p_start_cluster(p_dopar_condition(n_iterations > 250))

    batch <- 0
    offset <- sample.int(2^24, 1)
    for (distr.x in distribution.x) {
      for (distr.y in distribution.y) {
        for (ceil in ceiling) {
          for (sample_size in n) {
            for (effect.loop in effect) {
              for (slope.loop in slope) {
                intercept <- p_intercept(slope.loop, corner, effect.loop)

                cat("\r", strrep(" ", 100),
                    "\rDoing batch", batch + 1, "of", paste(n_batch, ""))

                r <- NULL
                ids <- seq(1, rep, max(1, round(rep / 30)))
                sig_results <- foreach(r = 1:rep, .combine = c) %dopar% {
                  set.seed(batch * rep + r + offset)
                  if (r %in% ids) {
                    cat(".")
                  }

                  df <- nca_random(sample_size, intercept, slope.loop, corner = corner,
                                   distribution.x = distr.x, distribution.y = distr.y)
                  capture.output(
                    model <- nca_analysis(df, 1, 2, ceilings = ceil, corner = corner,
                                          test.rep = test.rep, scope = c(0, 1, 0, 1))
                  )

                  # Estimated p-value
                  pval <- nca_extract(model, 'X', ceil, 'p-value')
                  # Power
                  return(as.numeric(pval <= p))
                }

                # Store the results for this iteration
                df <- data.frame(n = sample_size, ES = effect.loop,
                                 slope = slope.loop, ceiling = ceil,
                                 distribution.x = distr.x, distribution.y = distr.y,
                                 power = mean(sig_results))
                results <- rbind(results, df)

                batch <- batch + 1
              }
            }
          }
        }
      }
    }
    cat("\r", strrep(" ", 120), "\r\n")

    p_stop_cluster()
    Sys.unsetenv(p_skip_purity)

    return(results)
  }

p_intercept <-
  function (slope, corner, effect) {
    if (corner == 1) {
      intercept <- p_intercept_1_4(slope, effect)
    }
    else if (corner == 2) {
      intercept <- p_intercept_2_3(slope, effect)
    }
    else if (corner == 3) {
      intercept <- p_intercept_2_3(slope, 1 - effect)
    }
    else if (corner == 4) {
      intercept <- p_intercept_1_4(slope, 1 - effect)
    }
    return(intercept)
  }

p_intercept_1_4 <-
  function (slope, effect) {
    # Assume intercept >= 0, line through right, y on x == 1 should be < 1
    intercept <- 1 - effect - slope / 2
    if (intercept >= 0 && (intercept + slope) <= 1) {
      return(intercept)
    }

    # Assume intercept >= 0, line through roof, y on x == 1 should be >= 1
    intercept <- 1 - sqrt(2 * effect * slope)
    if (intercept >= 0 && (intercept + slope) >= 1) {
      return(intercept)
    }

    # Assume intercept < 0, line through roof, y on x == 1 should be >= 1
    intercept <- 0.5 - effect * slope
    if (intercept < 0 && (intercept + slope) >= 1) {
      return(intercept)
    }

    # Assume intercept < 0, line through side, y on x == 1 should be <> 1
    y <- sqrt(2 * (1 - effect) * slope)
    intercept <- y - slope
    return(intercept)
  }

p_intercept_2_3 <-
  function (slope, effect) {
    # Assume intercept <= 1, line through right, y on x == 1 should be >= 0
    intercept <- 1 - effect - (slope / 2)
    if (intercept <= 1 && (intercept + slope) >= 0) {
      return(intercept)
    }

    # Assume intercept <= 1, line through floor, y on x == 0 should be < 0
    intercept <- sqrt(2 * (effect - 1) * slope)
    if (intercept <= 1 && (intercept + slope) <= 0) {
      return(intercept)
    }

    # Assume intercept > 1, line through side, y on x == 0 should be >= 1
    intercept <- 1 - slope - sqrt(-2 * slope * effect)
    if (intercept > 1 && (intercept + slope) >= 0) {
      return(intercept)
    }

    # # Assume intercept > 1, line through floor, y on x == 0 should be <> 1
    intercept <- 1 / 2 - slope * (1 - effect)
    return(intercept)
  }

nca_powerplot <-
  function (df_power,
            x.variable = 'n', x.name = "Sample size",
            x.min = 0, x.max = NULL,
            line.variable = 'ES', line.name = "Effect size") {
    # Reduce vectorized variabes to single values (always first)
    has.warning <- FALSE
    plot.variables <- c('power', x.variable, line.variable)
    for (name in setdiff(colnames(df_power), plot.variables)) {
      values <- unique(df_power[[name]])
      if (length(values) != 1) {
        cat("\nVariable '", name, "' is vectorized: ", toString(values))
        cat("\nUsing only '", name, "' = ", values[1])
        df_power <- df_power[df_power[[name]] == values[1],]
        has.warning <- TRUE
      }
    }
    if (has.warning) {
      cat("\n")
    }

    if (is.null(x.min)) {
      x.min <- min(df_power[[x.variable]])
    }
    if (is.null(x.max)) {
      x.max <- max(df_power[[x.variable]])
    }

    p_new_window(title = "Power plot")
    df_power$var2 <- as.character(df_power[[line.variable]])
    b <- ggplot(df_power, aes(x = .data[[x.variable]], y = .data$power, col = .data$var2))
    b <- b + xlim(x.min, x.max) + ylim(0, 1) + geom_line()
    b <- b + labs(x = x.name, y = "Power", colour = line.name)
    b <- b + theme(legend.key = element_rect(fill = "white"))
    print(b)
    return(b)
  }
