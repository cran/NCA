p_display_plotly <-
function (plot, peers, labels) {
  # Get the params for plotting
  params <- get_plot_params()
  line_colors <- params[[1]]
  line_types <- params[[2]]
  lineWidth <- params[[3]] / 2
  point_type <- params[[4]]
  point_color <- params[[5]]

  # Missing (or too much) labels
  if (is.null(labels) || length(labels) != length(plot$x) || length(unique(labels)) > 5) {
    labels <- replicate(length(plot$x), 'obs')
    color.list <- c('blue')
  } else {
    color.list <- c("blue", "green3", "cyan", "magenta", "gray")
  }

  fig <- plot_ly(colors = color.list)

  # Add peers as separate trace first
  fig <- add_trace(fig, x = c(peers[, 1]), y = c(peers[, 2]),
                   text = rownames(peers), type = 'scatter', mode = 'markers',
                   marker = list(color = 'red', size = 10),
                   hovertemplate = '<b>%{text}</b><br>%{x} %{y}',
                   showlegend = TRUE, name = 'peer')

  # Add the scatter plot on top of peers, use colors if wanted
  df <- data.frame(x = c(plot$x), y = c(plot$y), labels = labels)
  fig <- add_trace(fig, data = df, x = ~x, y = ~y, text = rownames(plot$x),
                   type = 'scatter', mode = 'markers',
                   marker = list(symbol = point_type),
                   color = ~as.factor(labels), showlegend = TRUE,
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
      if (is_infinite(line)) {
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
  # TODO If we add subset to model, limit range to theo, range=c(50, 100)
  # fig <- layout(fig, title = paste0("<br/>NCA Plot : ", plot$title),
  fig <- layout(fig,
                # title = list(text = paste0("NCA Plot : ", plot$title), y = 0.99, yref="container"),
                title = list(text = paste0("NCA Plot : ", plot$title), yanchor = "top"),
                xaxis = list(title = colnames(plot$x)),
                yaxis = list(title = colnames(plot$y))
  )

  print(fig)
}

p_allColor <-
function (x) {
  result <- sapply(x, function (X) {
    tryCatch(is.matrix(col2rgb(X)),
             error = function (e) FALSE)
  })

  return(all(result))
}