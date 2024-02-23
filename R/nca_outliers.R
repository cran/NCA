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

  model <- nca_analysis(data, x, y, ceilings = ceiling, corner = corner,
                        flip.x = flip.x, flip.y = flip.y, scope = scope)
  eff.or <- model$summaries[[1]]$params[2]
  global <- model$summaries[[1]]$global
  params <- list(model = model, x = x, y = y, ceiling = ceiling,
                 corner = corner, flip.x = flip.x, flip.y = flip.y,
                 scope = scope, eff.or = eff.or, global = global,
                 min.dif = min.dif, peers = p_aggregate_peers(model$peers, 1))

  org_outliers <- p_get_outliers(data, params, 1)
  if (is.null(org_outliers)) {
    message("\nNo outliers identified")
    return()
  }
  outliers <- p_format_outliers(org_outliers, max.results, 1, min.dif, condensed)

  if (plotly) {
    points <- data[outliers[, 1], c(x, y)]
    labels <- paste0(rownames(points), '<br>diff ', outliers[, 5])
    marks <- paste0(outliers[, 6], outliers[, 7])
    points <- cbind(points, labels = labels, marks)
    points <- points[points[, 4] != '', 1:3]
    p_display_plotly(model$plots[[1]], points, NULL, name = 'outlier')
  }

  if (k == 1) {
    # If k == 1, all outliers are shown in plotly so we have to slice here
    return(outliers[1:min(nrow(outliers), max.results),])
  }

  outliers <- p_get_outliers(data, params, k, org_outliers)
  return(p_format_outliers(outliers, max.results, k, min.dif, condensed))
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
  if (ceiling == "ols") {
    message()
    message("Outlier detection does not work with OLS")
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
  condition <- detectCores() > 2 && nrow(combos) > 250
  p_start_cluster(condition)
  if (condition) {
    message("Starting the analysis on ", detectCores(), " cores")
  }
  else {
    registerDoSEQ()
  }

  idx <- NULL
  ids <- seq(1, nrow(combos), max(1, round(nrow(combos) / 50)))
  outliers <- foreach(idx = 1:nrow(combos), .combine = rbind) %dopar% {
    if (condition && idx %in% ids) {
      cat(".")
    }
    return(p_get_outlier(data, combos[idx,], params, k))
  }
  if (condition) {
    message("\rDone", strrep(' ', 50), "\n")
  }

  if (is.null(outliers)) {
    return (org_outliers)
  }

  new_outliers <- foreach(idx = 1:nrow(outliers), .combine = rbind) %dopar% {
    outlier <- outliers[idx, ]
    old_combo <- outlier[8]
    tmp <- org_outliers

    #max.dif.rel <- max(abs(unlist(tmp[tmp[, "combo"] %in% unlist(old_combo), 5])))
    #if (abs(unlist(outlier[5])) != max.dif.rel) {
    #   return()
    #}

    f <- function (n) {
      dif.rel <- tmp[tmp[, "combo"] %in% n,]$dif.rel
      return (abs(ifelse(is.null(dif.rel), 0, dif.rel)))
    }

    ord <- unlist(sapply(unlist(old_combo), f))
    tmp <- list(unlist(old_combo)[order(-ord)])
    outlier[1] <- p_get_names(unlist(tmp), k)$outliers

    return (outlier)
  }

  stopImplicitCluster()

  return(rbind(org_outliers, new_outliers))
}


p_get_all_names <- function (data, peers, global, params, k) {
  all.names <- rownames(peers)
  tmp.var <- c(params$x, params$x, params$y, params$y)
  tmp.scope <- c(3, 4, 5, 6)
  for (idx in 1:4) {
    tmp <- data[data[[tmp.var[idx]]] %in% global[tmp.scope[idx]],]
    if (k >= nrow(tmp)) {
      all.names <- c(all.names, rownames(tmp))
    }
  }

  return (all.names)
}


p_get_combos <- function (data, params, k) {
  # For COLS and QR we need all the points
  if (params$ceiling[1] %in% p_no_peer_line) {
    return(t(combn(rownames(data), k)))
  }

  all.names <- p_get_all_names(data, params$peers, params$global, params, k)

  counter <- k
  while (counter > 1) {
    data <- data[!(row.names(data) %in% all.names),]
    model <- nca_analysis(data, params$x, params$y, ceilings = params$ceiling,
                          corner = params$corner, flip.x = params$flip.x,
                          flip.y = params$flip.y, scope = params$scope)

    global <- model$summaries[[1]]$global
    all.names <- c(
      all.names,
      p_get_all_names(data, p_aggregate_peers(model$peers, 1), global, params, k)
    )

    counter <- counter - 1
  }

  all.names <- unique(all.names)
  if (k == 1) {
    return (matrix(all.names, ncol = 1))
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
  eff.nw <- model.new$summaries[[1]]$params[2]
  dif.abs <- ifelse(is.na(eff.nw), 0, eff.nw - params$eff.or)
  dif.rel <- ifelse(params$eff.or < epsilon, 0, 100 * dif.abs / params$eff.or)
  global.new <- model.new$summaries[[1]]$global
  return(list(eff.nw, dif.abs, dif.rel, global.new))
}


p_zone_scope <- function (combo, params, global.new) {
  idx <- ncol(global.new)
  found_scope <- !all(global.new[2:6, idx] == params$global[2:6, idx])

  if (params$ceiling[1] %in% p_no_peer_line) {
    found_ceiling <- T
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
  if (nrow(outliers) == 0) {
    return()
  }

  outliers <- data.frame(outliers)

  for (i in 1:7) { outliers[[i]] <- unlist(outliers[[i]]) }
  for (row_idx in 1:nrow(outliers)) {
    len <- length(unlist(strsplit(outliers[row_idx, 1], ' - ', fixed=T)))
    outliers[row_idx, 9] <- len
  }
  outliers <- outliers[order(-abs(outliers$dif.rel), outliers[, 9]),]
  org.length <- nrow(outliers)

  # If k == 1: all outliers are shown on Plotly
  if (k != 1) {
    # Strip combos where extra observations do not change outcome
    if (nrow(outliers) > 1 && condensed) {
      keep <- 1
      # Remove outliers which are not larger than previous (but keep singles)
      for (row_idx in 2:nrow(outliers)) {
        parts <- unlist(strsplit(outliers[row_idx, 1], ' - ', fixed=T))
        current <- abs(outliers[row_idx, 5])
        prev <- abs(outliers[row_idx - 1, 5])
        if (length(parts) == 1 || abs(current - prev) > min.dif) {
          keep <- c(keep, row_idx)
        }
      }
      outliers <- outliers[keep, ]
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
  class(outliers) <- c("outliers", class(outliers))

  rownames(outliers) <- NULL
  return(outliers)
}


print.outliers <- function (x, ...) {
  # NextMethod()
  print(format(x, justify = "left"))

  hidden <- attr(x, HIDDEN)
  shown <- attr(x, SHOWN)
  # Only show when 'full' outliers, not when selection
  if (!is.null(hidden) && shown == nrow(x)) {
    message(sprintf("# Not showing %s possible outliers", hidden))
  }
}
