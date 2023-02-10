nca_outliers <- function (data, x, y, ceiling = NULL,
                          corner = NULL, flip.x = FALSE, flip.y = FALSE,
                          k = 1, min.dif = 1e-2, max.results = 25,
                          plotly = FALSE) {
  # Cleans up any cluster registration
  p_cluster_cleanup()

  input.ok <- p_check_input(x, y, ceiling)
  if (isFALSE(input.ok)) {
    return()
  }
  if (is.null(input.ok)) {
    ceiling <- "ce_fdh"
  }

  model <- nca_analysis(data, x, y, ceilings = ceiling,
                        corner = corner, flip.x = flip.x, flip.y = flip.y)
  eff.or <- model$summaries[[1]]$params[2]
  global <- model$summaries[[1]]$global
  params <- list(model = model, x = x, y = y, ceiling = ceiling,
                 corner = corner, flip.x = flip.x, flip.y = flip.y,
                 eff.or = eff.or, global = global,
                 min.dif = min.dif,
                 peers = model$peers[[1]])

  org_outliers <- p_get_outliers(data, params, 1)
  outliers <- p_format_outliers(org_outliers, max.results, 1)
  if (is.null(org_outliers)) {
    message("\nNo outliers identified")
    return()
  }

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
  return(p_format_outliers(outliers, max.results, k))
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
  outliers <- foreach(idx = 1:nrow(combos), .combine = 'rbind') %dopar% {
    if (condition && idx %in% ids) {
      cat(".")
    }
    return(p_get_outlier(data, combos[idx,], params, k))
  }
  if (condition) {
    message("\rDone", strrep(' ', 50))
  }

  if (condition) {
    message("Finalizing the analysis")
  }
  ids <- round(seq(1, nrow(outliers), nrow(outliers) / 50))
  new_outliers <- foreach(idx = 1:nrow(outliers), .combine = rbind
      ) %dopar% {
    if (condition && idx %in% ids) {
      cat(".")
    }
    outlier <- outliers[idx, ]
    old_combo <- outlier[8]
    tmp <- org_outliers

    #max.dif.rel <- max(abs(unlist(tmp[tmp[, "combo"] %in% unlist(old_combo), 5])))
    #if (abs(unlist(outlier[5])) != max.dif.rel) {
    #   return()
    #}

    f <- function (n) {
      dif.rel <- tmp[tmp[, "combo"] %in% n, ]$dif.rel
      return( abs(ifelse(is.null(dif.rel), 0, dif.rel)) )
    }
    ord <- unname(unlist(sapply(unlist(old_combo), f)))
    new_combo <- old_combo[order(-ord)]
    outlier[8] <- new_combo[!sapply(new_combo, is.null)]
    outlier[1] <- p_get_names(unlist(outlier[8]), k)$outliers

    return ( outlier )
  }
  if (condition) {
    message("\rDone", strrep(' ', 50))
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

  return( all.names )
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
    model <- nca_analysis(data, params$x, params$y,
                          ceilings = params$ceiling, corner = params$corner,
                          flip.x = params$flip.x, flip.y = params$flip.y)

    global <- model$summaries[[1]]$global
    all.names <- c(
      all.names,
      p_get_all_names(data, model$peers[[1]], global, params, k)
    )

    counter <- counter - 1
  }

  all.names <- unique(all.names)
  combos <- t(combn(all.names, k))

  if (k == 1) {
    combos <- matrix(combos, ncol = 1)
  }

  return(combos)
}


p_get_outlier <- function (data, combo, params, k) {
  data.new <- data[-(which(rownames(data) %in% combo)),]
  values <- p_get_values(data.new, params)
  eff.nw <- values[1]; dif.abs <- values[2]; dif.rel <- values[3]

  if (round(abs(dif.rel), digits = 2) < params$min.dif) {
    return()
  }

  zone_scope <- p_zone_scope(data, combo, params)
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
                            flip.x = params$flip.x, flip.y = params$flip.y)
  eff.nw <- model.new$summaries[[1]]$params[2]
  dif.abs <- ifelse(is.na(eff.nw), 0, eff.nw - params$eff.or)
  dif.rel <- ifelse(params$eff.or < epsilon, 0, 100 * dif.abs / params$eff.or)
  return(c(eff.nw, dif.abs, dif.rel))
}


p_zone_scope <- function (data, combo, params) {
  found_scope <- any(data[combo, params$x] %in% params$global[c(3, 4)]) ||
    any(data[combo, params$y] %in% params$global[c(5, 6)])

  if (params$ceiling[1] %in% p_no_peer_line) {
    found_zone <- T
  }
  else {
    found_zone <- any(combo %in% rownames(params$peers))
  }

  return(c(ifelse(found_zone, "X", ""), ifelse(found_scope, "X", "")))
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


p_format_outliers <- function (outliers, max.results, k) {
  # The dataset / param combination doesn't produce outliers
  if (nrow(outliers) == 0) {
    return()
  }

  outliers <- data.frame(outliers)
  outliers <- outliers[, names(outliers) != "combo"]
  for (i in 1:length(outliers)) { outliers[[i]] <- unlist(outliers[[i]]) }
  outliers <- outliers[order(-abs(outliers$dif.rel)),]
  outliers[, c(2, 3, 4)] <- round(outliers[, c(2, 3, 4)], digits = 2)
  outliers[, 5] <- round(outliers[, 5], digits = 1)

  if (k != 1) {
    # If k == 1: all outliers are shown on Plotly
    outliers <- outliers[1:min(nrow(outliers), max.results),]
    fmt <- paste0("%- ", max(nchar(outliers$outliers)), "s")
    outliers$outliers <- sprintf(fmt, trimws(outliers$outliers, which = "left"))
  }

  rownames(outliers) <- NULL

  return(outliers)
}
