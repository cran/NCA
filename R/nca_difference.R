nca_difference <-
  function (data1, data2 = NULL, x, y,
            ceilings = c("ce_fdh", "cr_fdh"), scope = NULL, test.rep = 1000,
            test.type = c("contrast", "independent", "paired")) {
    test.type <- match.arg(test.type)
    p_test_difference_args(data1, data2, x, y, test.rep, test.type)
    if (test.type == "contrast") {
      data2 <- data1
    }
    else {
      x <- c(x, x)
    }
    ceilings <- p_validate_ceilings(ceilings)
    scopes <- p_scope(x, scope)

    # Always do parallel
    if (p_dopar_condition(test.rep >= 50)) {
      p_setenv(p_parallel_force)
      p_start_cluster(TRUE)
    }
    # Never do purity
    p_setenv(p_skip_purity)

    differences <- list()
    for (ceiling in ceilings) {
      # Compute observations
      observations <- p_observed(data1, data2, x, y, ceiling, scopes)
      obs <- observations$obs
      obs1 <- observations$obs1
      obs2 <- observations$obs2

      # Compute p-value
      if (test.rep > 0) {
        perm <- p_perm_test(data1, data2, x, y, ceiling, scopes, test.rep, test.type)
        tmp <- abs(perm) >= abs(obs)
        p_value <- (sum(tmp) + 1) / (length(tmp) + 1)
        p_value <- max(p_value, 1/test.rep)
      }
      else {
        p_value <- NA
      }

      differences[[ceiling]] <- list(
        observed1 = obs1, observed2 = obs2, observed = obs, p_value = p_value)
    }

    Sys.unsetenv(p_parallel_start)
    Sys.unsetenv(p_parallel_force)
    p_stop_cluster()
    Sys.unsetenv(p_skip_purity)

    attr(differences, "test.type") <- test.type
    attr(differences, "x") <- x
    class(differences) <- c("nca_difference", class(differences))

    return(differences)
  }

p_test_difference_args <-
  function (data1, data2, x, y, test.rep, test.type) {
    # --- correctness assertions (not robustness) ---
    if (test.type == "contrast") {
      if (length(x) != 2) {
        stop("For 'contrast', all 'x' must be a vector with 2 values.", call. = F)
      }
      if (p_invalid_variable(x, data1)) {
        stop("For 'contrast', all 'x' must be columns in data1.", call. = F)
      }
      if (p_invalid_variable(y, data1)) {
        stop("For 'contrast', 'y' must be a column in data1.", call. = F)
      }
    }
    if (test.type == "independent") {
      if (length(x) != 1) {
        stop("For 'independent', 'x' must be a single column name.", call. = F)
      }
      if (p_invalid_variable(x, data1) || p_invalid_variable(x, data2)) {
        stop("For 'independent', both data1 and data2 must",
             "contain columns 'x'.", call. = F)
      }
      if (p_invalid_variable(y, data1) || p_invalid_variable(y, data2)) {
        stop("For 'independent', both data1 and data2 must",
             "contain columns 'y'.", call. = F)
      }
      if (!identical(names(data1), names(data2))) {
        stop("For 'independent', data1 and data2 must have",
             "identical column sets and order (so rbind aligns).", call. = F)
      }
    }
    if (test.type == "paired") {
      if (length(x) != 1) {
        stop("For 'paired', 'x' must be a single column name.", call. = F)
      }
      if (p_invalid_variable(x, data1) || p_invalid_variable(x, data2)) {
        stop("For 'paired', both data1 and data2 must",
             "contain columns 'x'.", call. = F)
      }
      if (p_invalid_variable(y, data1) || p_invalid_variable(y, data2)) {
        stop("For 'paired', both data1 and data2 must",
             "contain columns 'y'.", call. = F)
      }
      if (nrow(data1) != nrow(data2)) {
        stop("For 'paired', data1 and data2 must have the same number of ",
             "rows (same cases).", call. = F)
      }
      if (!identical(names(data1), names(data2))) {
        stop("For 'paired', data1 and data2 must ",
             "have identical column sets and order.", call. = F)
      }
    }

    if (test.rep <= 0) {
      warning("No permutation test: test.rep is 0")
    }
  }

p_observed <-
  function (data1, data2, x, y, ceiling, scopes) {
    model1 <- nca_analysis(data1, x[1], y, ceilings = ceiling, scope = scopes[[1]])
    effect1 <- model1$`Effect size`[[1]][[1]]
    model2 <- nca_analysis(data2, x[2], y, ceilings = ceiling, scope = scopes[[2]])
    effect2 <- model2$`Effect size`[[1]][[1]]

    obs1 <- effect1
    obs2 <- effect2

    return(list(obs = obs2 - obs1, obs1 = obs1, obs2 = obs2))
  }

p_perm_test <-
  function (data1, data2, x, y, ceiling, scopes, test.rep, test.type) {
    ids <- seq(1, test.rep, max(1, round(test.rep / 50)))
    cat(sprintf("Doing %-6s ", ceiling))

    i <- NULL
    perm <- foreach(i = 1:test.rep) %dopar% {
      if (i %in% ids) {
        cat(".")
      }
      if (test.type == "contrast") {
        # Permutation logic: randomly swap X1 and X2 within each case.
        data1_perm <- data1
        swap_mask <- sample(c(TRUE, FALSE), nrow(data1), replace = TRUE)
        # temp <- data1_perm[[x[1]]]
        # data1_perm[[x[1]]][swap_mask] <- data1_perm[[x[2]]][swap_mask]
        # data1_perm[[x[2]]][swap_mask] <- temp[swap_mask]
        data1_perm[swap_mask, c(x[1], x[2])] <- data1_perm[swap_mask, c(x[2], x[1])]
        data2_perm <- data1_perm
      }
      else if (test.type == "independent") {
        # Permutation logic: randomly shuffle group labels across pooled sample
        combined_data <- rbind(data1, data2)
        # perm_indices <- sample(nrow(combined_data))
        # data1_perm <- combined_data[perm_indices[1:nrow(data1)],]
        # data2_perm <- combined_data[perm_indices[(nrow(data1) + 1):nrow(combined_data)],]
        n1 <- nrow(data1); n2 <- nrow(data2)
        labels <- c(rep(1, n1), rep(2, n2))
        lab_perm <- sample(labels)
        data1_perm <- combined_data[lab_perm == 1, , drop = FALSE]
        data2_perm <- combined_data[lab_perm == 2, , drop = FALSE]
      }
      else if (test.type == "paired") {
        # Permutation logic: randomly swap time labels within each case
        # sign_flip <- sample(c(-1, 1), nrow(data1), replace = TRUE)
        # data1_perm <- data1
        # data2_perm <- data1 + sign_flip * (data2 - data1)
        flips <- sample(c(TRUE, FALSE), nrow(data1), replace = TRUE)
        data1_perm <- data1
        data2_perm <- data2
        data1_perm[flips,] <- data2[flips,]
        data2_perm[flips,] <- data1[flips,]
      }

      model1 <- nca_analysis(data1_perm, x[1], y, ceilings = ceiling, scope = scopes[[1]])
      eff1 <- model1$`Effect size`[[1]][[1]]
      model2 <- nca_analysis(data2_perm, x[2], y, ceilings = ceiling, scope = scopes[[2]])
      eff2 <- model2$`Effect size`[[1]][[1]]

      return(eff2 - eff1)
    }
    cat("\n")

    return(unlist(perm))
  }

print.nca_difference <-
  function (x, ...) {
    differences <- x
    test.type <- attr(x, "test.type")
    x <- attr(differences, "x")
    ceilings <- names(differences)
    if (test.type != "contrast") {
      x <- paste0(x, c(".1", ".2"))
    }

    simple <- matrix(nrow = 3, ncol = 2 * length(differences))
    col.names <- NULL
    for (i in seq_along(differences)) {
      ceiling <- ceilings[i]
      difference <- differences[[ceiling]]
      col.names <- c(col.names, ceiling, 'p')

      simple[1, 2 * i - 1] <- round(difference$observed1, digits = 2)
      simple[2, 2 * i - 1] <- round(difference$observed2, digits = 2)
      simple[3, 2 * i - 1] <- round(difference$observed, digits = 2)
      simple[3, 2 * i] <- round(difference$p_value, digits = 3)
    }
    colnames(simple) <- col.names
    rownames(simple) <- c(x, 'Difference')

    cat("\n", strrep('-', dash.count), "\n", sep = "")
    message("Effect size (difference):")
    print(simple, na.print = "", quote = FALSE)
    cat(strrep('-', dash.count), "\n\n", sep = "")
  }

p_invalid_variable <-
  function (vv, df) {
    invalid <- FALSE
    for (v in vv) {
      if (is.numeric(v)) {
        invalid <- invalid || (v < 1 || v > ncol(df))
      }
      else {
        invalid <- invalid || !(v %in% names(df))
      }
    }
    return(invalid)
  }
