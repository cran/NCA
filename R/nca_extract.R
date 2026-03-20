p_add_extracts <-
  function (model) {
    for (idx in seq_along(p_GLOBAL_NAMES)) {
      name <- p_GLOBAL_NAMES[idx]
      model[[name]] <- lapply(model$summaries, function (s) { return(s$global[idx,]) })
    }

    for (idx in seq_along(p_PARAM_NAMES)) {
      name <- p_PARAM_NAMES[idx]
      if (name == " ") {
        next
      }
      model[[name]] <- lapply(model$summaries, function (s) { return(as.list(s$params[idx,])) })
    }

    return(model)
  }

nca_extract <-
  function (model, x = NULL, ceiling = NULL, param = 'Effect size') {
    # Validate X
    if (is.null(x)) {
      x <- names(model$summaries)[1]
      cat(paste0("'x' is not defined, using '", x, "'\n\n"))
    }
    names <- names(model$summaries)
    if (!(x %in% names)) {
      stop(x, " is not a valid independent variable name\n",
           "  Options: '", paste(names, collapse = "' '"), "'")
    }

    # Validate ceiling
    if (is.null(ceiling)) {
      ceiling <- colnames(model$summaries[[1]]$params)[1]
      cat(paste0("'ceiling' is not defined, using '", ceiling, "'\n\n"))
    }
    names <- colnames(model$summaries[[1]]$params)
    if (!(ceiling %in% names)) {
      stop(ceiling, " is not a valid ceiling name\n",
           "  Options: '", paste(names, collapse = "' '"), "'")
    }

    # Validate param
    names <- setdiff(c(p_GLOBAL_NAMES, p_PARAM_NAMES), " ")
    if (!(param %in% names)) {
      stop(param, " is not a valid param name\n",
           "  Options: '", paste(names, collapse = "' '"), "'")
    }

    if (param %in% p_GLOBAL_NAMES) {
      global <- model$summaries[[x]]$global
      row.idx <- match(param, rownames(global))
      return(global[row.idx])
    }
    else {
      params <- model$summaries[[x]]$params
      row.idx <- match(param, rownames(params))
      col.idx <- match(ceiling, colnames(params))
      return(params[[row.idx, col.idx]])
    }
  }
