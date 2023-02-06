p_display_plotly <-
function (plot, peers, labels, name = 'peer') {
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

  # Add peers as separate trace first
  peer_names <- rownames(peers)
  if (ncol(peers) > 2) {
    peer_names <- peers[, 3]
  }
  fig <- add_trace(fig, x = peers[, 1], y = peers[, 2],
                   text = peer_names, type = 'scatter', mode = 'markers',
                   marker = list(color = 'red', size = 10),
                   hovertemplate = '<b>%{text}</b><br>%{x}, %{y}',
                   showlegend = TRUE, name = name)

  # Add the scatter plot for the remaining points, use colors if wanted
  include <- !(rownames(plot$x) %in% rownames(peers))
  fig <- add_trace(fig, x = plot$x[include], y = plot$y[include],
                   text = rownames(plot$x)[include],
                   color = ~as.factor(labels[include]),
                   type = 'scatter', mode = 'markers',
                   marker = list(symbol = point_type), showlegend = TRUE,
                   hovertemplate = '<b>%{text}</b><br>%{x}, %{y}')

  # Print the lines
  for (method in plot$methods) {
    line <- plot$lines[[method]]
    line_color <- line_colors[[method]]
    line_type <- line_types[[method]]
    line_list <- list(color = line_color, width = lineWidth,
                      dash = paste0(line_type, 'px'))

    if (method %in% p_ceilings_step) {
      fig <- add_trace(fig, type = 'scatter', mode = 'lines',
                       x = c(line[[1]]), y = c(line[[2]]),
                       line = line_list, showlegend = TRUE, name = method)
    } else {
      if (is_infinite(line) || is.null(line)) {
        next
      }
      if (is.double(line)) {
        intercept <- line[1]
        slope <- line[2]
      } else {
        intercept <- unname(coef(line)["(Intercept)"])
        slope <- unname(coef(line)["x"])
      }

      # Points from X scope
      scope <- plot$scope.theo
      y1 <- intercept + slope * scope[1]
      y2 <- intercept + slope * scope[2]
      # Points from Y scope
      x3 <- (scope[3] - intercept) / slope
      x4 <- (scope[4] - intercept) / slope
      df <- data.frame(x = c(scope[1], scope[2], x3, x4),
                       y = c(y1, y2, scope[3], scope[4]))
      # Filter points outside scope
      df <- df[df$x >= (scope[1] - epsilon) & df$x <= (scope[2] + epsilon),]
      df <- df[df$y >= (scope[3] - epsilon) & df$y <= (scope[4] + epsilon),]
      fig <- add_lines(fig, x = df$x, y = df$y,
                       line = line_list, showlegend = TRUE, name = method)
    }
  }

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

  p_suppress_warnings({print(fig)})
}

p_allColor <-
function (x) {
  result <- sapply(x, function (X) {
    tryCatch(is.matrix(col2rgb(X)),
             error = function (e) FALSE)
  })

  return(all(result))
}

p_suppress_warnings <- function(.expr) {
  txt <- "Can't display both discrete & non-discrete data on same axis"
  eval.parent(substitute(
    withCallingHandlers(.expr, warning = function(w) {
      cond <- startsWith(conditionMessage(w), txt)
      if (cond) {
        invokeRestart("muffleWarning")
      }
    })
  ))
}
