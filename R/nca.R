nca <-
function (data, x, y, ceilings=c("ols", "ce_fdh", "cr_fdh")) {
  model <- nca_analysis(data, x, y, ceilings=ceilings)
  attr(model, "show.plots") <- TRUE

  model["bottlenecks"] <- NULL
  model["tests"] <- NULL
  model["test.time"] <- NULL

  return( model )
}

nca_analysis <-
function (data, x, y, ceilings=c("ols", "ce_fdh", "cr_fdh"),
          flip.x=FALSE, flip.y=FALSE, scope=NULL,
          bottleneck.x='percentage.range', bottleneck.y='percentage.range',
          steps=10, step.size=NULL, cutoff=0, qr.tau=0.95,
          test.rep=0, test.p_confidence=0.95, test.p_threshold=0) {

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
  tests <- list()
  peers <- list()
  test.time <- 0

  # Loop the independent varaibles
  for (id.x in 1:length(data.x)) {
    loop.data <- p_create_loop_data(data.x, data.y, scope, flip.x, flip.y, id.x, qr.tau)
    p_warn_percentage_max(loop.data, bn.data)
    x.name <- loop.data$names[id.x]

    # We need this for the 'FIT' number, regardless of user preference
    analisys_ce_fdh <- p_nca_ce_fdh(loop.data, bn.data)
    loop.data$ce_fdh_ceiling <- analisys_ce_fdh$ceiling
    loop.data$ce_fdh_peers <- analisys_ce_fdh$peers
    analisys_ce_fdh$peers <- NULL

    # We need to make sure ce_cm_conf (if present) comes before cr_cm_conf
    if ("ce_cm_conf" %in% ceilings) {
      analisys_ce_cm_conf <- p_nca_ce_cm_conf(loop.data, bn.data)
      loop.data$ce_cm_conf_columns <- attr(analisys_ce_cm_conf$line, "columns")
    }

    analyses <- list()
    for (ceiling in ceilings) {
      if (ceiling == "ce_fdh") {
        analysis <- analisys_ce_fdh
      } else if (ceiling == "ce_cm_conf") {
        analysis <- analisys_ce_cm_conf
      } else {
        analysis <- do.call(paste0("p_nca_", ceiling), list(loop.data, bn.data))
      }
      if (!is.null(analysis$bottleneck) && !(ceiling %in% p_no_bottleneck)) {
        bn.data$bottlenecks[[ceiling]][x.name] <- analysis$bottleneck
      }
      analysis$bottleneck <- NULL
      analyses[[ceiling]] <- analysis
    }

    test.params <- list(rep=test.rep,
                        p_confidence=test.p_confidence,
                        p_threshold=test.p_threshold)
    test_tuple <- p_test(analyses, loop.data, test.params)
    if (!is.null(test_tuple)) {
      tests[[x.name]] <- test_tuple$test
      test.time <- test.time + test_tuple$test.time
    }

    # Add P-value/accuracy for displaying in summary
    for (ceiling in ceilings) {
      if (ceiling %in% names(tests[[x.name]])) {
        analyses[[ceiling]]$p <- tests[[x.name]][[ceiling]]$p
        analyses[[ceiling]]$p_accuracy <- tests[[x.name]][[ceiling]]$test.params$p_accuracy
      }
      else {
        analyses[[ceiling]]$p <- NA
        analyses[[ceiling]]$p_accuracy <- NA
      }
    }
    plots[[x.name]] <- p_plot(analyses, loop.data)
    summaries[[x.name]] <- p_summary(analyses, loop.data)
    peers[[x.name]] <- p_peers(loop.data)
  }

  model <- list(plots=plots,
                summaries=summaries,
                bottlenecks=bn.data$bottlenecks,
                peers=peers,
                tests=tests,
                test.time=p_test_time(test.time))
  class(model) <- "nca_result"
  attr(model, "show.plots") <- FALSE

  return ( model )
}
