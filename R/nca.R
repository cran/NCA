nca <-
function (data, x, y, ceilings=c("ols", "ce_fdh", "cr_fdh")) {
  model <- nca_analysis(data, x, y, ceilings=ceilings)
  attr(model, "suppress.output") <- FALSE

  return( model )
}

nca_analysis <-
function (data, x, y, ceilings=c("ols", "ce_fdh", "cr_fdh"),
          flip.x=FALSE, flip.y=FALSE, scope=NULL, weighting=FALSE,
          bottleneck.x='percentage.range', bottleneck.y='percentage.range',
          steps=10, step.size=NULL, cutoff=0) {

  # Validate and clean data
  cleaned <- p_validate_clean(data, x, y)
  data.x <- cleaned$x
  data.y <- cleaned$y

  # Validate ceiling types
  ceilings <- p_validate_ceilings(ceilings)

  # Validate flip.x and flip.y
  flip.x <- p_validate_flipx(x, flip.x)
  flip.y <- isTRUE(flip.y)

  # Validate scope
  scope <- p_scope(x, scope)

  # Data object for bottlenecks
  bn.data <- p_bottleneck_data(data.x, data.y, scope, flip.y, ceilings,
                               bottleneck.x, bottleneck.y, steps, step.size, cutoff)

  # Create output lists
  plots <- list()
  summaries <- list()

  # Loop the independent varaibles
  for (id.x in 1:length(data.x)) {
    loop.data <- p_create_loop_data(data.x, data.y, scope, flip.x, flip.y, id.x, weighting)
    p_warn_percentage_max(loop.data, bn.data)
    x.name <- loop.data$names[id.x]

    analyses <- list()
    if ("ols" %in% ceilings) {
      analysis <- p_nca_ols(loop.data, bn.data)
      analysis$bottleneck <- NULL
      analyses$ols <- analysis
    }
    if ("lh" %in% ceilings) {
      analysis <- p_nca_lh(loop.data, bn.data)
      bn.data$bottlenecks$lh[x.name] <- analysis$bottleneck
      analysis$bottleneck <- NULL
      analyses$lh <- analysis
    }
    if ("cols" %in% ceilings) {
      analysis <- p_nca_cols(loop.data, bn.data)
      bn.data$bottlenecks$cols[x.name] <- analysis$bottleneck
      analysis$bottleneck <- NULL
      analyses$cols <- analysis
    }
    if ("qr" %in% ceilings) {
      analysis <- p_nca_qr(loop.data, bn.data)
      bn.data$bottlenecks$qr[x.name] <- analysis$bottleneck
      analysis$bottleneck <- NULL
      analyses$qr <- analysis
    }
    if ("ce_vrs" %in% ceilings) {
      analysis <- p_nca_ce_vrs(loop.data, bn.data)
      bn.data$bottlenecks$ce_vrs[x.name] <- analysis$bottleneck
      analysis$bottleneck <- NULL
      analyses$ce_vrs <- analysis
    }
    if ("cr_vrs" %in% ceilings) {
      analysis <- p_nca_cr_vrs(loop.data, bn.data)
      bn.data$bottlenecks$cr_vrs[x.name] <- analysis$bottleneck
      analysis$bottleneck <- NULL
      analyses$cr_vrs <- analysis
    }
    if ("ce_fdh" %in% ceilings) {
      analysis <- p_nca_ce_fdh(loop.data, bn.data)
      bn.data$bottlenecks$ce_fdh[x.name] <- analysis$bottleneck
      analysis$bottleneck <- NULL
      analyses$ce_fdh <- analysis
    }
    if ("cr_fdh" %in% ceilings) {
      analysis <- p_nca_cr_fdh(loop.data, bn.data)
      bn.data$bottlenecks$cr_fdh[x.name] <- analysis$bottleneck
      analysis$bottleneck <- NULL
      analyses$cr_fdh <- analysis
    }
    if ("sfa" %in% ceilings) {
      analysis <- p_nca_sfa(loop.data, bn.data)
      if (!is.null(analysis$bottleneck)) {
        bn.data$bottlenecks$sfa[x.name] <- analysis$bottleneck
      }
      analysis$bottleneck <- NULL
      analyses$sfa <- analysis
    }

    plots[[x.name]] <- p_plot(analyses, loop.data)
    summaries[[x.name]] <- p_summary(analyses, loop.data)
  }

  model <- list( plots=plots,
                 summaries=summaries,
                 bottlenecks=bn.data$bottlenecks )
  class(model) <- "nca_result"
  attr(model, "suppress.output") <- TRUE

  return ( model )
}