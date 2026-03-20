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
function (data, x, y, ceilings=c("ols", "ce_fdh", "cr_fdh"), custom=NULL,
          corner=NULL, flip.x=FALSE, flip.y=FALSE, scope=NULL,
          bottleneck.x='percentage.range', bottleneck.y='percentage.range',
          steps=10, step.size=NULL, cutoff=0, qr.tau=0.95, effect_aggregation=1,
          test.rep=0, test.p_confidence=0.95, test.p_threshold=0.05) {

  # Cleans up any cluster registration
  p_cluster_cleanup()

  # Validate and clean data
  cleaned <- p_validate_clean(data, x, y)
  data.x <- cleaned$x
  data.y <- cleaned$y

  # Validate ceiling types and custom if applicable
  custom <- p_validate_custom(custom, data.x)
  if (!is.null(custom)) {
    ceilings <- c(ceilings, p_ceiling_custom)
  }
  ceilings <- p_validate_ceilings(ceilings)

  # Overrule flip.x and flip.y if corners is defined
  if (!is.null(corner)) {
    corner <- p_validate_corner(x, corner)
    if (flip.x || flip.y) {
      warning("Ignoring 'flip.x' and 'flip.y': 'corner' is defined", call.=FALSE)
    }
    flip.y <- all(corner %in% c(3, 4))
    flip.x <- corner %in% c(2, 4)
  }
  # Always validate flip.x and flip.y
  flip.x <- p_validate_flipx(x, flip.x)
  flip.y <- isTRUE(flip.y)

  # Validate scope
  scope <- p_scope(x, scope)

  # Validate effect size aggregation
  effect_aggregation <- p_validate_effect_aggregation(effect_aggregation)

  # Data object for bottlenecks
  bn.data <- p_bottleneck_data(data.x, data.y, scope, flip.y, ceilings,
                               bottleneck.x, bottleneck.y, steps, step.size, cutoff)

  # Data for tests
  # test.rep can never be larger than n!, only test for small n's
  # fact(15) is 1e12, if user chooses that for test.rep he's got other problems
  if (nrow(data.y) < 16 && test.rep > factorial(nrow(data.x))) {
    test.rep <- factorial(nrow(data.y))
    fmt <- "\nLowered test.rep to %s as it can not be larger than N!\n\n"
    cat(sprintf(fmt, test.rep))
  }
  test.params <- list(rep=test.rep,
                      p_confidence=test.p_confidence,
                      p_threshold=test.p_threshold)

  # Create cluster for parallisation if needed
  p_start_cluster(p_dopar_condition(
    length(ceilings) * length(data.x) * test.rep > 6000, min.cores=1))

  # Create output lists
  plots <- list()
  summaries <- list()
  tests <- list()
  peers <- list()
  test.time <- 0

  # Loop the independent variables
  for (id.x in seq_along(data.x)) {
    loop.data <- p_create_loop_data(data.x, data.y, scope, flip.x, flip.y, id.x, qr.tau, custom)
    p_warn_percentage_max(loop.data, bn.data)
    x.name <- loop.data$names[id.x]

    # We need this for the 'FIT' number, regardless of user preference
    analysis_ce_fdh <- p_nca_wrapper("ce_fdh", loop.data, bn.data, effect_aggregation)
    loop.data$ce_fdh_ceiling <- analysis_ce_fdh$ceiling
    loop.data$ce_fdh_peers <- analysis_ce_fdh$peers

    analyses <- list()
    for (ceiling in ceilings) {
      if (ceiling == "ce_fdh") {
        analysis <- analysis_ce_fdh
      } else {
        analysis <- p_nca_wrapper(ceiling, loop.data, bn.data, effect_aggregation)
      }
      if (!is.null(analysis$bottleneck) && !(ceiling %in% p_no_bottleneck)) {
        bn.data$bottlenecks[[ceiling]][x.name] <- analysis$bottleneck
        attr(bn.data$bottlenecks[[ceiling]][x.name], 'cases') <- attr(analysis$bottleneck, 'cases')
      }
      analysis$bottleneck <- NULL
      analyses[[ceiling]] <- analysis
      peers[[ceiling]][[x.name]] <- analysis$peers
    }

    test_tuple <- p_test(analyses, loop.data, test.params, effect_aggregation)
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
    plots[[x.name]] <- p_plot(analyses, loop.data, corner)
    summaries[[x.name]] <- p_summary(analyses, loop.data)
  }

  # Shut down cluster for parallisation
  p_stop_cluster()

  # Add the bottlenecks with mpy attribute
  bottlenecks <- bn.data$bottlenecks
  attr(bottlenecks, "mpy") <- bn.data$mpy
  attr(bottlenecks, "flip.y") <- flip.y

  model <- list(plots=plots,
                summaries=summaries,
                bottlenecks=bottlenecks,
                peers=peers,
                tests=tests,
                test.time=p_test_time(test.time))

  model <- p_add_extracts(model)

  class(model) <- "nca_result"
  attr(model, "show.plots") <- FALSE

  return ( model )
}
