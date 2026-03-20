HIDDEN <- 'hidden'
SHOWN <- 'shown'

nca_outliers <- function (data, x, y, ceiling = NULL,
                          corner = NULL, flip.x = FALSE, flip.y = FALSE,
                          scope = NULL,
                          k = 1, min.dif = 1e-2, max.results = 25,
                          plotly = FALSE, condensed = FALSE) {
  # Cleans up any cluster registration
  p_cluster_cleanup()

  input.ok <- p_check_input(x, y, ceiling)
  if (isFALSE(input.ok)) {
    return()
  }
  if (is.null(input.ok)) {
    ceiling <- "ce_fdh"
  }
  data <- as.data.frame(p_validate_clean(data, x, y, TRUE))
  x <- colnames(data)[1]
  y <- colnames(data)[2]

  # Never do purity
  p_setenv(p_skip_purity)

  model <- nca_analysis(data, x, y, ceilings = ceiling, corner = corner,
                        flip.x = flip.x, flip.y = flip.y, scope = scope)
  eff.or <- nca_extract(model, x, ceiling, 'Effect size')

  params <- list(model = model, x = x, y = y, ceiling = ceiling,
                 corner = corner, flip.x = flip.x, flip.y = flip.y,
                 scope = scope, eff.or = eff.or,
                 Scope = nca_extract(model, x, ceiling, 'Scope'),
                 x.min = nca_extract(model, x, ceiling, 'Xmin'),
                 x.max = nca_extract(model, x, ceiling, 'Xmax'),
                 y.min = nca_extract(model, x, ceiling, 'Ymin'),
                 y.max = nca_extract(model, x, ceiling, 'Ymax'),
                 min.dif = min.dif, peers = p_aggregate_peers(model$peers, 1))

  org_outliers <- p_get_outliers(data, params, 1)
  if (k == 1 && is.null(org_outliers)) {
    message("\nNo outliers identified")
    Sys.unsetenv(p_skip_purity)
    return()
  }
  outliers <- p_format_outliers(org_outliers, max.results, 1, min.dif, condensed)

  if (plotly && !is.null(outliers)) {
    points <- data[outliers[, 1], c(x, y)]
    labels <- paste0(rownames(points), '<br>diff ', outliers[, 5])
    marks <- paste0(outliers[, 6], outliers[, 7])
    points <- cbind(points, labels = labels, marks)
    points <- points[points[, 4] != '', 1:3]
    p_display_plotly(model$plots[[1]], points, NULL, name = 'outlier')
  }

  if (k == 1) {
    # If k == 1, all outliers are shown in plotly so we have to slice here
    outliers <- outliers[1:min(nrow(outliers), max.results),]
    # Don't show 'scope' column if a theoretical scope is defined
    if (!is.null(scope)) {
      outliers <- outliers[, names(outliers) != 'scope']
    }
    Sys.unsetenv(p_skip_purity)
    return(outliers)
  }

  outliers <- p_get_outliers(data, params, k, org_outliers)
  if (is.null(outliers) || nrow(outliers) == 0) {
    message("\nNo outliers identified")
    Sys.unsetenv(p_skip_purity)
    return()
  }

  outliers <- p_format_outliers(outliers, max.results, k, min.dif, condensed)
  # Don't show 'scope' column if a theoretical scope is defined
  if (!is.null(scope)) {
    outliers <- outliers[, names(outliers) != 'scope']
  }
  Sys.unsetenv(p_skip_purity)
  return(outliers)
}


p_check_input <- function (x, y, ceiling) {
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

  tmp <- c(p_ceilings_step, p_ceilings_line)
  allowed <- tmp[tmp != "ols"]
  if (!(ceiling %in% allowed)) {
    message()
    message(paste("Outlier detection does not work with", ceiling))
    return(FALSE)
  }

  return(TRUE)
}


p_get_outliers <- function (data, params, k, org_outliers = NULL) {
  if (k > length(rownames(data))) {
    k <- length(rownames(data))
    warning(paste("Reduced k to", length(rownames(data))), call. = F)
  }
  combos <- p_get_combos(data, params, k)

  # Start a cluster if needed
  condition <- p_dopar_condition(nrow(combos) > 250)
  p_start_cluster(condition)

  idx <- NULL
  ids <- seq(1, nrow(combos), max(1, round(nrow(combos) / 50)))
  outliers <- foreach(idx = seq_len(nrow(combos)), .combine = rbind) %dopar% {
    if (condition && idx %in% ids) {
      cat(".")
    }
    return(p_get_outlier(data, combos[idx,], params, k))
  }
  if (condition) {
    message("\rDone", strrep(' ', 50), "\n")
  }

  if (is.null(outliers)) {
    p_stop_cluster()

    return(org_outliers)
  }

  new_outliers <- foreach(idx = seq_len(nrow(outliers)), .combine = rbind) %dopar% {
    outlier <- outliers[idx,]
    old_combo <- outlier[8]
    tmp <- org_outliers

    f <- function (n) {
      dif.rel <- tmp[tmp[, "combo"] %in% n,]$dif.rel
      return(abs(ifelse(is.null(dif.rel), 0, dif.rel)))
    }

    ord <- unlist(sapply(unlist(old_combo), f))
    tmp <- list(unlist(old_combo)[order(-ord)])
    outlier[1] <- p_get_names(unlist(tmp), k)$outliers

    return(outlier)
  }

  p_stop_cluster()

  return(rbind(org_outliers, new_outliers))
}


p_get_all_names <- function (data, peers, params, k) {
  all.names <- rownames(peers)
  tmp.var <- c(params$x, params$x, params$y, params$y)
  scope <- c(params$x.min, params$x.max, params$y.min, params$y.max)
  for (idx in 1:4) {
    tmp <- data[data[[tmp.var[idx]]] %in% scope[[idx]],]
    if (k >= nrow(tmp)) {
      all.names <- c(all.names, rownames(tmp))
    }
  }

  return(all.names)
}


p_get_combos <- function (data, params, k) {
  # For COLS and QR we need all the points
  if (params$ceiling[1] %in% p_no_peer_line) {
    return(t(combn(rownames(data), k)))
  }

  all.names <- p_get_all_names(data, params$peers, params, k)

  counter <- k
  while (counter > 1) {
    data <- data[!(row.names(data) %in% all.names),]
    model <- nca_analysis(data, params$x, params$y, ceilings = params$ceiling,
                          corner = params$corner, flip.x = params$flip.x,
                          flip.y = params$flip.y, scope = params$scope)

    all.names <- c(
      all.names,
      p_get_all_names(data, p_aggregate_peers(model$peers, 1), params, k)
    )

    counter <- counter - 1
  }

  all.names <- unique(all.names)
  if (k == 1) {
    return(matrix(all.names, ncol = 1))
  }

  combos <- NULL
  counter <- k
  while (counter > 1) {
    tmp <- t(combn(all.names, counter))
    tmp <- cbind(tmp, matrix(NA, ncol = k - counter, nrow = nrow(tmp)))
    combos <- rbind(combos, tmp)
    counter <- counter - 1
  }

  return(combos)
}


p_get_outlier <- function (data, combo, params, k) {
  combo <- combo[!is.na(combo)]
  data.new <- data[-(which(rownames(data) %in% combo)),]
  values <- p_get_values(data.new, params)
  eff.nw <- values[[1]]; dif.abs <- values[[2]]; dif.rel <- values[[3]]
  global.new <- values[[4]]

  if (round(abs(dif.rel), digits = 2) < params$min.dif) {
    return()
  }

  zone_scope <- p_zone_scope(combo, params, global.new)
  zone <- zone_scope[1]; scope <- zone_scope[2]

  # Extra check: 'ceiling' outliers must be on COLS and C_LP lines
  if (all(k == 1, params$ceiling[1] %in% c("cols", "c_lp"), scope == "")) {
    line <- params$model$plots[[1]]$lines[[1]]
    if (params$ceiling == "cols") {
      line <- unname(coef(line))
    }
    intercept <- line[1]
    slope <- line[2]
    x.value <- data[combo[k], params$x]
    y.value <- data[combo[k], params$y]
    y.calc <- intercept + x.value * slope

    # If not on line, ignore ceiling zone outlier
    if (all.equal(y.value, y.calc) != TRUE) {
      return()
    }
  }

  names <- p_get_names(combo, k)
  values <- list(eff.or = params$eff.or, eff.nw = eff.nw,
                 dif.abs = dif.abs, dif.rel = dif.rel,
                 ceiling = zone, scope = scope, combo = combo)
  return(append(names, values))
}


p_get_values <- function (data.new, params) {
  model.new <- nca_analysis(data.new, params$x, params$y,
                            ceilings = params$ceiling, corner = params$corner,
                            flip.x = params$flip.x, flip.y = params$flip.y,
                            scope = params$scope)
  eff.nw <- nca_extract(model.new, params$x, params$ceiling, 'Effect size')

  dif.abs <- ifelse(is.na(eff.nw), 0, eff.nw - params$eff.or)
  zero.dif.rel <- ifelse(dif.abs < epsilon, 0, Inf)
  dif.rel <- ifelse(params$eff.or < epsilon, zero.dif.rel, 100 * dif.abs / params$eff.or)
  global.new <- c(
    nca_extract(model.new, params$x, params$ceiling, 'Scope'),
    nca_extract(model.new, params$x, params$ceiling, 'Xmin'),
    nca_extract(model.new, params$x, params$ceiling, 'Xmax'),
    nca_extract(model.new, params$x, params$ceiling, 'Ymin'),
    nca_extract(model.new, params$x, params$ceiling, 'Ymax')
  )

  return(list(eff.nw, dif.abs, dif.rel, global.new))
}


p_zone_scope <- function (combo, params, global.new) {
  global.org <- c(params$Scope,
                  params$x.min, params$x.max, params$y.min, params$y.max)
  found_scope <- !all(global.new == global.org)

  if (params$ceiling[1] %in% p_no_peer_line) {
    found_ceiling <- TRUE
  }
  else {
    found_ceiling <- any(combo %in% rownames(params$peers))
  }

  return(c(ifelse(found_ceiling, "X", ""), ifelse(found_scope, "X", "")))
}


p_get_names <- function (combo, k) {
  if (k == 1) {
    name <- combo[1]
  }
  else {
    name <- paste(combo, '-', collapse = " ")
    name <- substr(name, 0, nchar(name) - 2)
  }
  return(list(outliers = name))
}


p_format_outliers <- function (outliers, max.results, k, min.dif, condensed) {
  # The dataset / param combination doesn't produce outliers
  if (is.null(outliers) || nrow(outliers) == 0) {
    return()
  }

  outliers <- data.frame(outliers)

  for (i in 1:7) { outliers[[i]] <- unlist(outliers[[i]]) }
  for (row_idx in seq_len(nrow(outliers))) {
    len <- length(unlist(strsplit(outliers[row_idx, 1], ' - ', fixed = T)))
    outliers[row_idx, 9] <- len
  }
  outliers <- outliers[order(-abs(outliers$dif.rel), -abs(outliers$dif.abs), outliers[, 9]),]
  org.length <- nrow(outliers)

  # If k == 1: all outliers are shown on Plotly
  if (k != 1) {
    # Strip combos where extra observations do not change outcome
    if (nrow(outliers) > 1 && condensed) {
      keep <- 1
      # Remove outliers which are not larger than previous (but keep singles)
      for (row_idx in 2:nrow(outliers)) {
        parts <- unlist(strsplit(outliers[row_idx, 1], ' - ', fixed = T))
        current <- abs(outliers[row_idx, 5])
        prev <- abs(outliers[row_idx - 1, 5])
        if (length(parts) == 1 || abs(current - prev) > min.dif) {
          keep <- c(keep, row_idx)
        }
      }
      outliers <- outliers[keep,]
    }

    outliers <- outliers[1:min(nrow(outliers), max.results),]
  }

  outliers <- outliers[, -c(8, 9)]
  outliers[, c(2, 3, 4)] <- round(outliers[, c(2, 3, 4)], digits = 2)
  outliers[, 5] <- round(outliers[, 5], digits = 1)

  if (org.length > nrow(outliers)) {
    attr(outliers, SHOWN) <- nrow(outliers)
    attr(outliers, HIDDEN) <- org.length - nrow(outliers)
  }
  class(outliers) <- c("nca_outliers", class(outliers))

  rownames(outliers) <- NULL
  return(outliers)
}


print.nca_outliers <- function (x, ...) {
  # NextMethod()
  print(format(x, justify = "left"))

  hidden <- attr(x, HIDDEN)
  shown <- attr(x, SHOWN)
  # Only show when 'full' outliers, not when selection
  if (!is.null(hidden) && shown == nrow(x)) {
    message(sprintf("# Not showing %s possible outliers", hidden))
  }
}
