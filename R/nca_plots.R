p_plot <-
  function (analyses, loop.data, corner) {
    plot <- list()

    plot$x <- loop.data$x
    plot$y <- loop.data$y
    plot$scope.theo <- loop.data$scope.theo
    plot$names <- loop.data$names
    plot$flip.x <- ifelse(is.null(corner), loop.data$flip.x, FALSE)
    plot$flip.y <- ifelse(is.null(corner), loop.data$flip.y, FALSE)
    plot$conf <- loop.data$conf
    plot$title <- p_generate_title(colnames(plot$x), colnames(plot$y))
    plot$methods <- names(analyses)
    plot$lines <- list()
    plot$line_matrices <- list()

    for (method in plot$methods) {
      analysis <- analyses[[method]]
      plot$lines[[method]] <- analysis$line
      plot$line_matrices[[method]] <- analysis$line_matrix
    }

    return(plot)
  }

p_display_plot <-
  function (plot, bottleneck.list, pdf = FALSE, path = NULL) {
    # Get the params for plotting
    params <- get_plot_params()
    line_colors <- params[[1]]
    line_types <- params[[2]]
    line_width <- params[[3]]
    point_type <- params[[4]]
    point_color <- params[[5]]
    point_size <- params[[6]]

    # Open new window or PDF
    if (pdf) {
      p_new_pdf("plot", plot$title, path)
    } else {
      p_new_window(title = plot$title)
      par(family = "")
      par(mfrow = c(1, 1))
    }

    # Add extra width / height for bottleneck data
    xlim <- c(plot$scope.theo[1 + plot$flip.x], plot$scope.theo[2 - plot$flip.x])
    ylim <- c(plot$scope.theo[3 + plot$flip.y], plot$scope.theo[4 - plot$flip.y])
    if (!is.null(names(bottleneck.list$hor))) {
      xlim <- c(xlim[1] - diff(xlim) / 10, xlim[2])
    }
    if (!is.null(names(bottleneck.list$ver))) {
      ylim <- c(ylim[1] - diff(ylim) / 25, ylim[2])
    }

    # Create main data frame
    df_points <- data.frame(x = plot$x, y = plot$y)
    names(df_points) <- c("x", "y")

    # Plot the data points
    p <- ggplot() +
      geom_point(data = df_points, aes(x = .data[['x']], y = .data[['y']]),
                 shape = point_type, color = point_color, size = point_size, fill = NA) +
      labs(x = colnames(plot$x), y = tail(plot$names, n = 1),
           title = paste0("NCA Plot : ", plot$title)) +
      coord_cartesian(xlim = xlim, ylim = ylim) +
      theme(
        plot.title = element_text(hjust = 0.5, vjust = 2, size = 14),
        text = element_text(size = 12),
        axis.text = element_text(size = 12),

        panel.border = element_rect(colour = "black", fill = NA),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),

        legend.position = c(0.00125, 0.9989),
        legend.justification = c(0, 1),
        legend.background = element_rect(fill = "white"),
        legend.box.background = element_rect(colour = "black", linewidth=.5),
        legend.title = element_blank(),
        legend.key.height = unit(0.45, "cm"),
        legend.text = element_text(size = 8),
      )

    # Plot the scope outline
    p <- p_ggplot_outline(p, plot)

    # Plot the bottlenecks
    p <- p_ggplot_bottlenecks(p, bottleneck.list, plot$flip.x, plot$flip.y)

    # Process Lines
    lines_data <- data.frame()
    for (method in plot$methods) {
      line_matrix <- plot$line_matrices[[method]]
      if (is_infinite(line_matrix)) {
        next
      }
      tmp_df <- as.data.frame(line_matrix)
      names(tmp_df) <- c("x", "y")
      tmp_df$method <- p_pretty_name(method)
      tmp_df$color <- line_colors[[method]]
      tmp_df$line <- line_types[[method]]
      lines_data <- rbind(lines_data, tmp_df)
    }

    if (nrow(lines_data) > 0) {
      aesthetics <- unique(lines_data[c("method", "color", "line")])
      method_colors <- setNames(aesthetics$color, aesthetics$method)
      method_lines <- setNames(aesthetics$line, aesthetics$method)

      p <- p +
        geom_line(data = lines_data, linewidth = line_width,
                  aes(x = .data[['x']], y = .data[['y']],
                      color = .data[['method']], linetype = .data[['method']]))
      p <- p + scale_color_manual(values = method_colors)
      p <- p + scale_linetype_manual(values = method_lines)
    }

    print(p)
    if (pdf) {
      dev.off()
      cat("")
    }

    return(p)
  }

p_ggplot_bottlenecks <-
  function (p, bottleneck.list, flip.x, flip.y) {
    segments_hor <- NULL
    for (name in names(bottleneck.list[['hor']])) {
      coord <- unlist(bottleneck.list[['hor']][[name]])
      segments_hor <- rbind(segments_hor, coord)

      adjust <- abs(coord[2] - coord[1]) / 30
      coord.x <- ifelse(flip.x, coord[2] + adjust, coord[1] - adjust)
      p <- p + annotate(geom = "text", x = coord.x, y = coord[3], label = name, color = "grey")
    }
    if (!is.null(segments_hor)) {
      colnames(segments_hor) <- c("x", "xend", "y", "yend")
      p <- p + geom_segment(data = segments_hor, linetype = "dashed", color = "grey",
                            aes(x = .data[['x']], y = .data[['y']],
                                xend = .data[['xend']], yend = .data[['yend']]))
    }

    segments_ver <- NULL
    for (name in names(bottleneck.list[['ver']])) {
      coord <- unlist(bottleneck.list[['ver']][[name]])
      segments_ver <- rbind(segments_ver, coord)

      adjust <- abs(coord[4] - coord[3]) / 50
      coord.y <- ifelse(flip.y, coord[4] + adjust, coord[3] - adjust)
      p <- p + annotate(geom = "text", x = coord[1], y = coord.y, label = name, color = "grey")
    }
    if (!is.null(segments_ver)) {
      colnames(segments_ver) <- c("x", "xend", "y", "yend")
      p <- p + geom_segment(data = segments_ver, linetype = "dashed", color = "grey",
                            aes(x = .data[['x']], y = .data[['y']],
                                xend = .data[['xend']], yend = .data[['yend']]))
    }

    return(p)
  }

p_ggplot_outline <-
  function (p, plot) {
    p <- p + geom_vline(xintercept = plot$scope.theo[1], linetype = "dashed", color = "grey")
    p <- p + geom_vline(xintercept = plot$scope.theo[2], linetype = "dashed", color = "grey")
    p <- p + geom_hline(yintercept = plot$scope.theo[3], linetype = "dashed", color = "grey")
    p <- p + geom_hline(yintercept = plot$scope.theo[4], linetype = "dashed", color = "grey")
    return(p)
  }

get_plot_params <-
  function () {
    # Get the line colors and types from the global env, allowing user changes
    line_colors <- mget("line.colors", envir = .GlobalEnv, ifnotfound = "notfound")[[1]]
    line_types <- mget("line.types", envir = .GlobalEnv, ifnotfound = "notfound")[[1]]
    # Append default for missing values
    line_colors <- append(line_colors, line.colors)
    line_types <- append(line_types, line.types)
    # The same for line width and point-type/-color
    line_width <- mget("line.width", envir = .GlobalEnv, ifnotfound = "notfound")[[1]]
    if (!is.numeric(line_width)) {
      line_width <- line.width
    }
    point_type <- mget("point.type", envir = .GlobalEnv, ifnotfound = "notfound")[[1]]
    if (!is.numeric(point_type)) {
      point_type <- point.type
    }
    point_color <- mget("point.color", envir = .GlobalEnv, ifnotfound = "notfound")[[1]]
    point_color <- tryCatch({
      col2rgb(point_color); point_color
    }, error = function (e) return(point.color))
    point_size <- mget("point.SIZE", envir = .GlobalEnv, ifnotfound = "notfound")[[1]]
    if (!is.numeric(point_size)) {
      point_size <- point.size
    }

    return(list(line_colors, line_types, line_width, point_type, point_color, point_size))
  }

is_infinite <-
  function (line) {
    # LH line
    if (typeof(line) == "double" && !all(is.finite(line))) {
      return(TRUE)
    }
    # 'LM' style line
    if (typeof(line) == "list" && !all(is.finite(line$coefficients))) {
      return(TRUE)
    }
    return(FALSE)
  }
