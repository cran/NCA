nca_power <- function (n = c(20, 50, 100), effect = 0.10, slope = 1, ceiling = "ce_fdh", p = 0.05,
                       distribution.x = "uniform", distribution.y = "uniform",
                       rep = 100, test.rep = 200) {

  if (effect <= 0 || effect >= 1) {
    message("The effect size needs to be larger than 0 and smaller than 1\n")
    return()
  }
  else if (slope <= 0) {
    message("The slope needs to be larger than 0\n")
    return()
  }

  # Calculate the total number of iterations
  n_iterations <- (rep *
    length(distribution.x) *
    length(distribution.y) *
    length(n) *
    length(effect) *
    length(ceiling))

  # Define the variables that will store results
  results <- data.frame(n = numeric(), ES = numeric(), slope = numeric(),
                        ceiling = character(), p = numeric(),
                        distr.x = character(), distr.y = character(), power = numeric())

  # Counter of iterations (single samples)
  count <- 0
  for (distr.x in distribution.x) {
    for (distr.y in distribution.y) {
      for (ceil in ceiling) {
        for (sample_size in n) {
          for (effect_size in effect) {
            intercept <- p_intercept(slope, effect)

            # Initialize vectors to store p-values and power results
            pval <- numeric(rep)
            sig_results <- numeric(rep)

            for (r in 1:rep) {
              count <- count + 1
              cat("\rIteration ", count, " of ", n_iterations)

              df <- nca_random(sample_size, intercept, slope,
                               distribution.x = distr.x, distribution.y = distr.y)
              capture.output(
                model <- nca_analysis(df, 1, 2, ceilings = ceil,
                                      test.rep = test.rep, scope = c(0, 1, 0, 1))
              )

              # Estimated p-value
              pval[r] <- model$summaries$X$params[6]
              # Power
              sig_results[r] <- (pval[r] <= p)
            }

            # Store the results for this iteration
            df <- data.frame(n = sample_size, ES = effect_size, slope = slope,
                             ceiling = ceiling, p = mean(pval),
                             distr.x = distr.x, distr.y = distr.y,
                             power = mean(sig_results))
            results <- rbind(results, df)
          }
        }
      }
    }
  }
  cat("\n\n")

  return(results)
}

p_intercept <- function (slope, effect) {
  # Assume intercept >= 0, line through roof, y on x == 0 should be >= 1
  intercept <- 1 - sqrt(2 * effect * slope)
  if (intercept >= 0 && (intercept + slope) >= 1){
    return(intercept)
  }

  # Assume intercept >= 0, line through right, y on x == 0 should be < 1
  intercept <- 1 - effect - slope / 2
  if (intercept >= 0 && (intercept + slope) < 1){
    return (intercept)
  }

  # Assume intercept < 0, line through roof, y on x == 0 should be >= 1
  intercept <- 0.5 - effect * slope
  if (intercept < 0 && (intercept + slope) >= 1){
    return (intercept)
  }

  # Assume intercept < 0, line through side, y on x == 0 should be <> 1
  y <- sqrt(2 * (1 - effect) * slope)
  intercept <- y - slope
  return (intercept)
}
