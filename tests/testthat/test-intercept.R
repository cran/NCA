test_that("test p_intercept function", {
  # Only test manually, takes too long for building
  if (exists("p_intercept")) {
    return(NULL)
  }

  source("../../R/nca_power.R")

  df <- data.frame(x = c(0, 1, 0, 1), y = c(0, 0, 1, 1))
  corner_seq <- 1:4
  effect_seq <- seq(from = 0.01, to = 1, by = 0.01)
  slope_seq <- seq(from = -2, to = 2, by = 0.01)

  for (corner in corner_seq) {
    for (effect in effect_seq) {
      for (slope in slope_seq) {
        if (corner %in% c(1, 4) && slope < 0) {
          next
        }
        else if (corner %in% c(2, 3) && slope > 0) {
          next
        }
        if (slope == 0) {
          next
        }

        intercept <- p_intercept(slope, corner, effect)
        custom <- c(intercept, slope)
        model <- nca_analysis(df, 1, 2, ceilings = NULL,
                              custom = custom, corner = corner)
        calc <- model$`Effect size`[[1]][[1]]

        expect_equal(effect, calc, tolerance = 1e-6)
      }
    }
  }
})

