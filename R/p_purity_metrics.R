p_purity_metrics <-
  function (df, condition, outcome, ceiling) {
    # TODO Get from caller?
    scope.emp <- c(min(df[condition]), max(df[condition]),
                   min(df[outcome]), max(df[outcome]))
    fdh_peers <- p_get_peers(df, condition, outcome)
    vrs_peers <- p_get_peers(df, condition, outcome, vrs = TRUE)

    # Add case number, remove rownames, TODO Needed?
    caseNo <- seq(1, nrow(df))
    caseDescriptor <- rownames(df)
    df <- cbind(caseNo = caseNo, caseDescriptor = caseDescriptor, df)
    rownames(df) <- caseNo

    df <- p_corners(df, condition, outcome)
    xyO <- df[, c("caseNo", condition, outcome)]
    colnames(xyO) <- c('caseNo', 'x', 'y')

    if (ceiling == "ce_fdh") {
      stpceil <- p_makeStp(fdh_peers, scope.emp)
      stpweb <- p_make2SidedWeb(stpceil, scope.emp)

      meas <- p_sv4Points(xyO, stpweb)
      g_res <- p_svGauge(meas$compacts)
      gauge <- p_ensure_pct_cols(g_res$gauge, n_points = nrow(xyO))
      spread <- p_compute_spread(g_res$svP, stpweb)

      return(list(
        noise_pct = round(gauge$medPpct, 1),
        noise_nof = gauge$medP,
        exceptions_pct = round(100 * gauge$lowP / gauge$numC, 1),
        exceptions_nof = gauge$lowP,
        support_pct = round(gauge$medSsatDpct, 1),
        support_nof = gauge$medSsatD,
        spread = round(spread, 2),
        sharpness = round(gauge$sharp1, 2)
      ))
    }

    if (ceiling == "ce_vrs") {
      vrsceil <- p_makeVrs(vrs_peers, scope.emp)
      vrsweb <- p_make2SidedWeb(vrsceil, scope.emp)

      meas <- p_sv4Points(xyO, vrsweb)
      g_res <- p_svGauge(meas$compacts)
      gauge <- p_ensure_pct_cols(g_res$gauge, n_points = nrow(xyO))
      spread <- p_compute_spread(g_res$svP, vrsweb)

      return(list(
        noise_pct = round(gauge$medPpct, 1),
        noise_nof = gauge$medP,
        exceptions_pct = round(100 * gauge$lowP / gauge$numC, 1),
        exceptions_nof = gauge$lowP,
        support_pct = round(gauge$medSsatDpct, 1),
        support_nof = gauge$medSsatD,
        spread = round(spread, 2),
        sharpness = round(gauge$sharp1, 2)
      ))
    }

    if (ceiling == "cr_fdh") {
      linceil <- p_doLinCeil(fdh_peers, scope.emp, ilk = "RG")
      linweb <- p_make2SidedWeb(linceil, scope.emp)

      meas <- p_sv4Points(xyO, linweb)
      g_res <- p_svGauge(meas$compacts)
      gauge <- p_ensure_pct_cols(g_res$gauge, n_points = nrow(xyO))
      spread <- p_compute_spread(g_res$svP, linweb)

      return(list(
        noise_pct = round(gauge$medPpct, 1),
        noise_nof = gauge$medP,
        exceptions_pct = round(100 * gauge$lowP / gauge$numC, 1),
        exceptions_nof = gauge$lowP,
        support_pct = round(gauge$medSsatDpct, 1),
        support_nof = gauge$medSsatD,
        spread = round(spread, 2),
        sharpness = round(gauge$sharp1, 2)
      ))
    }

    if (ceiling == "c_lp") {
      elpceil <- p_doLinCeil(fdh_peers, scope.emp, ilk = "LP")
      elpweb <- p_make2SidedWeb(elpceil, scope.emp)

      meas <- p_sv4Points(xyO, elpweb)
      g_res <- p_svGauge(meas$compacts)
      gauge <- p_ensure_pct_cols(g_res$gauge, n_points = nrow(xyO))
      spread <- p_compute_spread(g_res$svP, elpweb)

      return(list(
        noise_pct = round(gauge$medPpct, 1),
        noise_nof = gauge$medP,
        exceptions_pct = round(100 * gauge$lowP / gauge$numC, 1),
        exceptions_nof = gauge$lowP,
        support_pct = round(gauge$medSsatDpct, 1),
        support_nof = gauge$medSsatD,
        spread = round(spread, 2),
        sharpness = round(gauge$sharp1, 2)
      ))
    }

    return(p_purity_empty)
  }

p_purity_empty <- list(
  noise_pct = NA,
  noise_nof = NA,
  exceptions_pct = NA,
  exceptions_nof = NA,
  support_pct = NA,
  support_nof = NA,
  spread = NA,
  sharpness = NA
)
