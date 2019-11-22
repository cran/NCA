p_plot <-
function (analyses, loop.data) {
  plot <- list()

  plot$x <- loop.data$x
  plot$y <- loop.data$y
  plot$scope.theo <- loop.data$scope.theo
  plot$names <- loop.data$names
  plot$flip.x <- loop.data$flip.x
  plot$flip.y <- loop.data$flip.y
  plot$conf <- loop.data$conf
  plot$title <- p_generate_title(colnames(plot$x), colnames(plot$y))
  plot$methods <- names(analyses)
  plot$lines <- list()

  for (method in plot$methods) {
    analysis <- analyses[[method]]
    if (is.null(analysis$line)) {
      next
    }
    plot$lines[[method]] <- analysis$line
  }

  return ( plot )
}

p_display_plot <-
function (plot, pdf=FALSE, path=NULL) {
  # Get the line colors and types from the global env, allowing user changes
  line_colors <- mget("line.colors", envir=.GlobalEnv, ifnotfound="notfound")[[1]]
  line_types <- mget("line.types", envir=.GlobalEnv, ifnotfound="notfound")[[1]]
  # Append default for missing values
  line_colors <- append(line_colors, line.colors)
  line_types <- append(line_types, line.types)
  # The same for line width and point-type/-color
  lineWidth <- mget("line.width", envir=.GlobalEnv, ifnotfound="notfound")[[1]]
  if (!is.numeric(lineWidth)) {
    lineWidth <- line.width
  }
  point_type <- mget("point.type", envir=.GlobalEnv, ifnotfound="notfound")[[1]]
  if (!is.numeric(point_type)) {
    point_type <- point.type
  }
  point_color <- mget("point.color", envir=.GlobalEnv, ifnotfound="notfound")[[1]]
  point_color <- tryCatch({
      col2rgb(point_color); point_color
  }, error = function(e) return(point.color) )

  # Open new window or PDF
  if (pdf) {
    p_new_pdf("plot", plot$title, path)
  } else {
    p_new_window(title=plot$title)
    par(family="")
    par(mfrow=c(1, 1))
  }

  # Plot the data points
  flip.x <- plot$flip.x
  flip.y <- plot$flip.y
  xlim <- c(plot$scope.theo[1 + flip.x], plot$scope.theo[2 - flip.x])
  ylim <- c(plot$scope.theo[3 + flip.y], plot$scope.theo[4 - flip.y])
  # Confidence lines might be outside the scope
  ylim <- p_con_lim(ylim, plot, flip.y)
  plot (plot$x, plot$y, pch=point_type, col=point_color,
        xlim=xlim, ylim=ylim,
        xlab=colnames(plot$x), ylab=tail(plot$names, n=1))

  # Plot the scope outline
  p_plot_outline(plot)

  # Plot a simple grid, only for development
  # p_plot_grid(plot, 10)

  # Plot the legend before adding the clipping area
  legendParams = list()
  for (method in plot$methods) {
    line_color <- line_colors[[method]]
    line_type  <- line_types[[method]]
    if (method %in% c("ce_cm_conf", "cr_cm_conf")) {
      long.name <- paste(p_pretty_name(method), plot$conf)
      legendParams$names  <- append(legendParams$names,  long.name)
    } else {
      legendParams$names  <- append(legendParams$names,  p_pretty_name(method))
    }
    legendParams$types <- append(legendParams$types,  line_type)
    legendParams$colors <- append(legendParams$colors, line_color)
  }
  if (length(legendParams) > 0) {
    legend("topleft", cex=0.7, legendParams$names,
           lty=legendParams$types, col=legendParams$colors, bg=NA)
  }

  # Apply clipping to the lines
  clip(xlim[1], xlim[2], ylim[1], ylim[2])

  # Print the lines
  for (method in plot$methods) {
    line <- plot$lines[[method]]
    line_color <- line_colors[[method]]
    line_type  <- line_types[[method]]

    if (method %in% p_ceilings_step) {
      lines(line[[1]], line[[2]], type="l",
            lty=line_type, col=line_color, lwd=lineWidth)
    } else {
      # LH line
      is_finite = TRUE
      if (typeof(line) == "double" && !all(is.finite(line))) {
        is_finite = FALSE
      }
      # 'LM' style line
      else if (typeof(line) == "list" && !all(is.finite(line$coefficients))) {
        is_finite = FALSE
      }
      if (is_finite) {
        abline(line, lty=line_type, col=line_color, lwd=lineWidth)
      }
    }

    # Only for development
    # p_plot_boundaries(line, method)
  }

  # Plot the title
  title <- paste0("NCA Plot : ", plot$title)
  title(title, cex.main=1)

  if (pdf) {
    dev.off()
    cat("")
  }
}

p_con_lim <-
function (ylim, plot, flip.y) {
  if ("ce_cm_conf" %in% plot$methods) {
    columns <- attr(plot$lines[["ce_cm_conf"]], "columns")
  } else if (("cr_cm_conf" %in% plot$methods)) {
    columns <- attr(plot$lines[["cr_cm_conf"]], "columns")
  } else {
    return ( ylim )
  }

  if (!flip.y) {
    ylim <- c(min(ylim[1], min(columns[5,])), max(ylim[2], max(columns[5,])))
  } else {
    ylim <- c(max(ylim[1], max(columns[5,])), min(ylim[2], min(columns[5,])))
  }

  return ( ylim )
}

p_plot_outline <-
function (plot) {
  abline(v=plot$scope.theo[1], lty=2, col="grey")
  abline(v=plot$scope.theo[2], lty=2, col="grey")
  abline(h=plot$scope.theo[3], lty=2, col="grey")
  abline(h=plot$scope.theo[4], lty=2, col="grey")
}

p_plot_grid_fixed <-
function (plot, size) {
  start <- size * ceiling(plot$scope.theo[1] / size)
  while (start < plot$scope.theo[2]) {
    abline(v=start, lty=1, col="grey")
    start <- start + size
  }
  start <- size * ceiling(plot$scope.theo[3] / size)
  while (start < plot$scope.theo[4]) {
    abline(h=start, lty=1, col="grey")
    start <- start + size
  }
}

p_plot_grid <-
function (plot, size) {
  step <- (plot$scope.theo[2] - plot$scope.theo[1]) / size
  start <- plot$scope.theo[1]
  while (start < plot$scope.theo[2]) {
    abline(v=start, lty=1, col="grey")
    start <- start + step
  }

  step <- (plot$scope.theo[4] - plot$scope.theo[3]) / size
  start <- plot$scope.theo[3]
  while (start < plot$scope.theo[4]) {
    abline(h=start, lty=1, col="grey")
    start <- start + step
  }
}

p_plot_boundaries <-
function (line, method) {
  columns <- attr(line, "columns")
  if (is.null(columns)) {
    return()
  }
  # Plot the column boundaries
  for (col in 1:ncol(columns)) {
    abline(v=columns[2, col], lty=2, col="grey")
  }
  abline(v=columns[3, col], lty=2, col="grey")
  # Plot the points used for the cr_cm_conf
  if (method == "cr_cm_conf") {
    points(t(columns[4:5,]), col="red")
  }
}
