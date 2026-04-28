p_display_plotly <-
  function (plot, peers, labels, name = 'peer', bottleneck.list = NULL) {
    # Get the params for plotting
    params <- get_plot_params()
    line_colors <- params[[1]]
    line_types <- params[[2]]
    lineWidth <- params[[3]] / 2
    point_type <- params[[4]]
    point_color <- params[[5]]

    # Missing (or too much) labels
    if (is.null(labels) ||
      length(labels) != length(plot$x) ||
      length(unique(labels)) > 5) {
      labels <- replicate(length(plot$x), 'obs')
      color.list <- 'blue'
    } else {
      color.list <- c("blue", "green3", "cyan", "magenta", "gray")
    }

    fig <- plot_ly(colors = color.list)

    template <- "<b>%s</b><br>%s, %s"
    agg_fun <- function (x) paste(unique(x), collapse = "<br>")
    text_fun <- function (r) { sprintf(template, r[3], r[1], r[2]) }

    # Add peers as separate trace first
    if (!is.null(peers)) {
      # https://github.com/plotly/plotly.R/issues/1859
      df <- data.frame(x = peers[, 1], y = peers[, 2])
      df$text <- rownames(peers)
      df <- aggregate(. ~ x + y, df, agg_fun)
      df$text <- apply(df, 1, text_fun)

      fig <- add_trace(fig, data = df, x = ~x, y = ~y,
                       type = 'scatter', mode = 'markers',
                       marker = list(color = 'red', size = 10),
                       hovertemplate = ~text,
                       showlegend = TRUE, name = name)
    }

    # Add the scatter plot for the remaining points, use colors if wanted
    include <- !(rownames(plot$x) %in% rownames(peers))

    df <- data.frame(x = plot$x[include], y = plot$y[include])
    df$label <- labels[include]
    df$text <- names(plot$x[include,])
    tmp <- aggregate(label ~ x + y, df, unique)
    df <- aggregate(text ~ x + y, df, agg_fun)
    df$text <- apply(df, 1, text_fun)
    df <- merge(df, tmp)

    fig <- add_trace(fig, data = df, x = ~x, y = ~y,
                     type = 'scatter', mode = 'markers',
                     marker = list(symbol = point_type),
                     hovertemplate = ~text,
                     showlegend = TRUE, color = df$label)

    # Plot the lines
    for (method in plot$methods) {
      if (method == "ols") {
        next
      }

      line <- plot$line_matrices[[method]]
      line_color <- line_colors[[method]]
      line_type <- line_types[[method]]
      line_list <- list(color = line_color, width = lineWidth,
                        dash = paste0(line_type, 'px'))

      if (is_infinite_null(line)) {
        next
      }

      fig <- add_trace(fig, type = 'scatter', mode = 'lines',
                       x = c(line[, 1]), y = c(line[, 2]),
                       line = line_list, showlegend = TRUE, name = method)
    }

    # Add the bottleneck lines
    # https://plotly.com/r/reference/layout/annotations/
    fig <- p_plotly_bottlenecks(fig, bottleneck.list)

    # Add title and axis labels
    title <- list(text = paste0("NCA Plot : ", plot$title), yanchor = "top")
    xaxis <- list(title = colnames(plot$x))
    if (plot$flip.x) {
      xaxis["autorange"] <- "reversed"
    }
    yaxis <- list(title = colnames(plot$y))
    if (plot$flip.y) {
      yaxis["autorange"] <- "reversed"
    }
    fig <- layout(fig, title = title, xaxis = xaxis, yaxis = yaxis)

    p_suppress_warnings({ print(fig) })
  }

p_allColor <-
  function (x) {
    result <- sapply(x, function (X) {
      tryCatch(is.matrix(col2rgb(X)),
               error = function (e) FALSE)
    })

    return(all(result))
  }

p_suppress_warnings <-
  function (.expr) {
    txt <- "Can't display both discrete & non-discrete data on same axis"
    eval.parent(substitute(
      withCallingHandlers(.expr, warning = function (w) {
        cond <- startsWith(conditionMessage(w), txt)
        if (cond) {
          invokeRestart("muffleWarning")
        }
      })
    ))
  }

p_plotly_bottlenecks <-
  function (fig, bottleneck.list) {
    for (name in names(bottleneck.list[['hor']])) {
      coord <- unlist(bottleneck.list[['hor']][[name]])
      line <- list(width = 1, dash = "dot", color = "lightgrey")
      fig <- add_lines(fig,
                       x = c(coord[1], coord[2]),
                       y = c(coord[3], coord[4]),
                       line = line, showlegend = F)
      a <- list(x = coord[1], y = coord[3],
                xref = "x", yref = "y", ax = -20, ay = -20,
                text = name, font = list(size = 10),
                arrowsize = 0.5, arrowwidth = 2, arrowcolor = "lightgrey")
      fig <- layout(fig, annotations = a)
    }
    for (name in names(bottleneck.list[['ver']])) {
      coord <- unlist(bottleneck.list[['ver']][[name]])
      line <- list(width = 1, dash = "dot", color = "lightgrey")
      fig <- add_lines(fig,
                       x = c(coord[1], coord[2]),
                       y = c(coord[3], coord[4]),
                       line = line, showlegend = F)
      a <- list(x = coord[1], y = coord[3],
                xref = "x", yref = "y", ax = -20, ay = 20,
                text = name, font = list(size = 10),
                arrowsize = 0.5, arrowwidth = 2, arrowcolor = "lightgrey")
      fig <- layout(fig, annotations = a)
    }
    return(fig)
  }
