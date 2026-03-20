p_corners <-
  function (df, condition, outcome) {
    peers <- p_get_peers(df, condition, outcome)

    # corner
    df$corner <- df$caseNo %in% rownames(peers)

    # start
    df$start <- FALSE
    for (peer.x in peers[, 1]) {
      same.x <- df[condition] == peer.x
      lowest.y <- df[outcome] == min(df[df[condition] == peer.x, outcome])
      df$start <- df$start | same.x & lowest.y
    }

    # peer
    highest.peer.idx <- as.integer(tail(rownames(peers), n = 1))
    df$peer <- data.frame(peer = rep(highest.peer.idx, nrow(df)))
    if (nrow(peers) >= 2) {
      for (row.idx in nrow(peers):2) {
        peer.x <- peers[row.idx, 1]
        peer.idx <- as.integer(rownames(peers)[row.idx - 1])
        df$peer[df[condition] < peer.x] <- peer.idx
      }
    }

    # size
    df$size <- NA
    counts <- table(df$peer)
    caseNos <- as.integer(names(counts))
    for (i in 1:nrow(counts)) {
      value <- counts[i]
      caseNo <- caseNos[i]
      df$size[df$caseNo == caseNo] <- value
    }

    return(df)
  }

p_get_peers <-
  function (df, condition, outcome, vrs = FALSE) {
    # TODO Flipping
    x <- as.matrix(df[condition])
    y <- as.matrix(df[outcome])
    loop.data <- list(x = x, y = y, flip.x = FALSE, flip.y = FALSE)
    return(p_peers(loop.data, vrs))
  }

p_fdh_nooks <-
  function (peers) {
    nooks <- matrix(nrow = 0, ncol = 2)
    for (i in 1:nrow(peers)) {
      peer <- peers[i,]
      # Add previous point if between peers
      if (i > 1) {
        nook.x <- peers[i, 1]
        nook.y <- peers[i - 1, 2]
        nooks <- rbind(nooks, c(nook.x, nook.y))
      }
      # Add the peer
      nooks <- rbind(nooks, peer)
    }

    rownames(nooks) <- NULL
    return(nooks)
  }

p_expand_fdh_nooks <-
  function (fdh_nooks) {
    if (nrow(fdh_nooks) == 0) {
      return(NULL)
    }

    fdh_nooks_expanded <- cbind(fdh_nooks, 1, 1)
    curr.col <- curr.row <- 1
    for (row.idx in 2:nrow(fdh_nooks_expanded)) {
      prev <- fdh_nooks_expanded[row.idx - 1,]
      curr <- fdh_nooks_expanded[row.idx,]
      if (prev[1] != curr[1]) {
        curr.row <- curr.row + 1
      }
      if (prev[2] != curr[2]) {
        curr.col <- curr.col + 1
      }
      fdh_nooks_expanded[row.idx, 3] <- curr.row
      fdh_nooks_expanded[row.idx, 4] <- curr.col
    }

    colnames(fdh_nooks_expanded) <- c("x", "y", "col", "row")
    return(fdh_nooks_expanded)
  }

p_crRank <-
  function (x) {
    # x: numericals to be ranked in col/row
    d <- diff(sort(x))
    d <- ifelse(d == 0, 0, 1)
    cd <- cumsum(d)   # my rank: cumulative sum
    r <- c(1, 1 + cd) # compensate for the one value lost due to differing
  }

p_addColRow2xyPoints <-
  function (xy) {
    if (nrow(xy) == 0) {
      return(NULL)
    }

    # possibly changes order of rows
    xy <- xy[order(xy$x, xy$y),]
    xy$col <- p_crRank(xy$x) # add column data
    xy$row <- p_crRank(xy$y) # add row data

    return(xy) # Warning: the order of rows possibly changed
  }

p_closingBr <-
  function (nooks, scope) {
    ceil <- xceil <- nooks
    peers_scope <- c(min(nooks[, 1]), max(nooks[, 1]), min(nooks[, 2]), max(nooks[, 2]))

    # projections
    if (peers_scope[3] > scope[3]) {
      # nooks does not meet the bottom (south side) of scope
      xceil <- rbind(c(peers_scope[1], scope[3]), xceil)
      if (peers_scope[1] > scope[1]) {
        ceil <- rbind(c(peers_scope[1], scope[3]), ceil)
      }
    }
    if (peers_scope[2] < scope[2]) {
      # nooks does not meet the east side of bb
      xceil <- rbind(xceil, c(scope[2], peers_scope[4]))
      if (peers_scope[4] < scope[4]) {
        ceil <- rbind(ceil, c(scope[2], peers_scope[4]))
      }
    }

    # add sw and ne corners of br (if not already there):
    if (length(xceil[xceil[, 1] == scope[1] & xceil[, 2] == scope[3],]) == 0) {
      # no sw so add
      xceil <- rbind(c(scope[1], scope[3]), xceil)
    }
    if (length(xceil[xceil[, 1] == scope[2] & xceil[, 2] == scope[4],]) == 0) {
      # no ne so add
      xceil <- rbind(xceil, c(scope[2], scope[4]))
    }

    return(list(ceil = ceil, xceil = xceil))
  }

p_creduceCeil <-
  function (nooks, scope) {
    south <- nooks[nooks[, 1] == scope[3],]
    west <- nooks[nooks[, 1] == scope[1],]
    if (length(south) == (2 * ncol(nooks)) || length(west) == (2 * ncol(nooks))) {
      # delete south-west (1st_pnt)
      nooks <- matrix(nooks[-1,], ncol = ncol(nooks))
    }

    north <- nooks[nooks[, 1] == scope[4],]
    east <- nooks[nooks[, 1] == scope[2],]
    if (length(north) == (2 * ncol(nooks)) || length(east) == (2 * ncol(nooks))) {
      # delete north-east
      nooks <- matrix(nooks[1:(nrow(nooks) - 1),], ncol = ncol(nooks))
    }

    # TODO I'm not sure this is correct: shouldn't this be the 'peers' scope?
    if (nrow(nooks) == 1) {
      # TODO Why not always?
      if ((nooks[1, 1] == scope[1]) & (nooks[1, 2] == scope[4])) {
        nooks <- NULL
      }
    }
    return(nooks)
  }

p_makeStp <-
  function (peers, scope) {
    if (nrow(peers) == 0) {
      xceil <- matrix(c(scope[1], scope[1], scope[2],
                        scope[3], scope[4], scope[4]), ncol = 2)
      ceilings <- list(ceil = NULL, xceil = xceil)
      return(ceilings)
    }

    nooks <- p_fdh_nooks(peers)
    ceilings <- p_closingBr(nooks, scope)
    ceil <- p_expand_fdh_nooks(ceilings$ceil)
    ceil <- p_creduceCeil(ceil, scope)
    xceil <- p_expand_fdh_nooks(ceilings$xceil)

    return(list(ceil = ceil, xceil = xceil))
  }

p_makeVrs <-
  function (vrs_peers, scope) {
    ceilings <- p_closingBr(vrs_peers, scope)
    ceil <- p_expand_fdh_nooks(ceilings$ceil)
    ceil <- p_creduceCeil(ceil, scope)
    xceil <- p_expand_fdh_nooks(ceilings$xceil)

    return(list(ceil = ceil, xceil = xceil))
  }

p_doLinCeil <-
  function (xyP, scope, ilk = "RG") {
    # determine line and bounding rectangle from xy points in xyP

    # we now have the raw values for a and b
    ab <- p_abCeil4Data(xyP, ilk = ilk)
    iLB <- p_intersectLineBox(ab, scope) # non-inflated
    # intersections: south-west (swi) and north-east (nei) intersections
    swi <- unname(iLB$swi) # on boundary of BB as both ab and BR derive from the same data
    nei <- unname(iLB$nei) # on boundary of BB as both ab and BR derive from the same data
    xyP <- unique(data.frame(x = c(swi[1], nei[1]), y = c(swi[2], nei[2])))
    ceilings <- p_closingBr(xyP, scope)
    # Note. For received linear cases the extended ceiling gets traced out by 4 points.

    # add col/row information on the points in the (x)ceiling:
    ceil <- p_addColRow2xyPoints(ceilings$ceil)
    ceil <- p_creduceCeil(ceil, scope)
    xceil <- p_addColRow2xyPoints(ceilings$xceil)

    deltay <- scope[4] - scope[3]
    a1 <- (ab["a"] - scope[3] + ab["b"] * scope[1]) / deltay
    b1 <- (scope[2] - scope[1]) * ab["b"] / deltay
    nrm <- c(a = a1, b = b1)

    return(ceilings = list(ceil = ceil, xceil = xceil, ab = list(raw = ab, nrm = nrm)))
  }

###############################################################################
# HERE START
###############################################################################
p_make2SidedWeb <-
  function (ceilings, scope) {
    # compute on meshes formed by the extended ceiling's break-points
    # Meshes (maze) form a partition of the bounding rectangle based on the breakpoints
    # in the extended ceiling.

    # Note: enlceil is a dataframe, not an matrix (mixed types)
    enlceil <- cbind(data.frame(ceilings$xceil), strain = "regular")

    # if enlargement then add points to xceil to enlarge it and obtain enlceil
    if (p_ENLARGEMENT_CONSTANT > 0) {
      dx <- scope[2] - scope[1]
      dy <- scope[4] - scope[3]
      extra.row <- data.frame(x = scope[1] - p_ENLARGEMENT_CONSTANT * dx / 2,
                              y = scope[3] - p_ENLARGEMENT_CONSTANT * dy / 2,
                              col = 0, row = 0, strain = "enlarged")
      enlceil <- rbind(extra.row, enlceil)

      tmp <- tail(enlceil, n = 1)
      extra.row <- data.frame(x = scope[2] + p_ENLARGEMENT_CONSTANT * dx / 2,
                              y = scope[4] + p_ENLARGEMENT_CONSTANT * dy / 2,
                              col = tmp[3] + 1, row = tmp[4] + 1, strain = "enlarged")
      enlceil <- rbind(enlceil, extra.row)
      enlceil[, 3:4] <- enlceil[, 3:4] + 1
      row.names(enlceil) <- NULL
    }

    ceilings$enlceil <- enlceil

    # xy (and col-row) raster with info
    ras <- p_twoSidedRaster(enlceil)
    rp <- ras$pnts

    # TODO Rename to scope.enl
    # bounding rectangle (enlarged) coordinates from xyCR (= xceil):
    br <- c(min(rp['x']), max(rp['x']), min(rp['y']), max(rp['y']))
    # TODO Rename to scopes
    BRs <- list(obr = scope, br = br)

    # Out of the raster points we build the mesh (= partition by mazes = rectangles)
    # Meshes: raster points (x, y) are considered as south-east points in a mesh
    # below procedure may give at first some incomplete/degenerate/non-existing
    # nw points/meshes which subsequently get removed
    colnames(rp) <- c('col', 'se_x', 'strainx', 'cardDirEW',
                      'row', 'se_y', 'strainy', 'cardDirNS', 'se_pos')

    # Now compute the meshes with current points in rp considered as se-points.
    r1 <- rp
    r1 <- cbind(r1, rp[, 'row'] - 1, rp[, 'col'] + 1)
    colnames(r1) <- c('col.y', 'nw_x', 'strainx', 'cardDirEW',
                      'row.y', 'nw_y', 'strainy', 'cardDirNS',
                      'nw_pos', 'row', 'col')

    # Remove "invalid" meshes - having corners or row-col with NA-values:
    rp <- rp[complete.cases(rp[, c("row", "col")]),]
    r1 <- r1[complete.cases(r1[, c("row", "col", "row.y", "col.y", "nw_x")]),]
    rp$insol <- NA
    rp$inver <- NA

    ribbon_db <- dbConnect(SQLite(), ":memory:")

    dbWriteTable(ribbon_db, "rp", rp)
    dbWriteTable(ribbon_db, "r1", r1)
    dbWriteTable(ribbon_db, "colrangebyrow", data.frame(ras$colrangebyrow))
    dbWriteTable(ribbon_db, "rowrangebycol", data.frame(ras$rowrangebycol))

    dbExecute(ribbon_db, CREATE_MAZE,
              params = list(br1 = br[1], br2 = br[2], br3 = br[3], br4 = br[4]))

    # Deviance areas to north-west
    dbExecute(ribbon_db, UPDATE_MAZE_1)

    # Deviance areas to south-east
    dbExecute(ribbon_db, UPDATE_MAZE_2)

    # Do lookup stuff (lm = lookup_maze)
    dbExecute(ribbon_db, UPDATE_MAZE_3)

    dbExecute(ribbon_db, UPDATE_MAZE_4,
              params = list(br1 = br[1], br2 = br[2], br3 = br[3], br4 = br[4]))

    dbExecute(ribbon_db, UPDATE_MAZE_5)

    dbExecute(ribbon_db, UPDATE_RP)

    maze <- dbReadTable(ribbon_db, "maze")
    ras$pnts <- dbReadTable(ribbon_db, "rp")
    dbDisconnect(ribbon_db)

    # effect size:
    se_maze <- maze[maze['s_row'] == min(maze['s_row']) & maze['e_col'] == max(maze['e_col']),]
    nces <- 1 - se_maze$se_insol

    dossier <- list(message = "", portrayal = "")
    if (nrow(maze) == 1) {
      if (maze$stance != "bisect") { # ceiling fully in bb boundary
        if (maze$stance == "nw") {
          TM1 <- "\n Single mesh: Empty space goes to south-east of bounding rectangle! \n"
          TM2 <- "There is no feasible space: in-veracity equals 1 (except on ceiling). \n"
          TM3 <- "Any iso-(in-)veracity-line will be meaningless! \n \n"
          dossier$message <- paste0(TM1, TM2, TM3)
          dossier$portrayal <- "no_feasible_space"
        } else { # maze$stance == "se"
          TM1 <- "\n Single mesh: Feasible point at the north-west of bounding rectangle! \n"
          TM2 <- "There is no empty space and in-solidity equals 1 (except on ceiling). \n"
          TM3 <- "Any iso-(in-)solidity-line will be meaningless! \n \n"
          dossier$message <- paste0(TM1, TM2, TM3)
          dossier$portrayal <- "no_empty_space"
        }
      }
    }

    return(list(ceilings = ceilings, raster = ras, BRs = BRs, enlarged = p_ENLARGEMENT_CONSTANT > 0,
                maze = maze, nces = nces, dossier = dossier))
  }

p_twoSidedRaster <-
  function (enlceil) {
    # raster from xy-column-rows (later the meshes)

    col.max <- max(enlceil['col'])
    row.max <- max(enlceil['row'])

    rangeby <- NULL
    for (row_idx in seq_len(nrow(enlceil))) {
      row <- enlceil[row_idx,]
      rows <- enlceil[enlceil['row'] == row$row,]
      cols <- enlceil[enlceil['col'] == row$col,]
      rangeby <- rbind(rangeby, c(row = row$row, col = row$col,
                                  x.low = min(rows['x']), x.high = max(rows['x']),
                                  y.low = min(cols['y']), y.high = max(cols['y']),
                                  col.low = min(rows['col']), col.high = max(rows['col']),
                                  row.low = min(cols['row']), row.high = max(cols['row'])))
    }
    rownames(rangeby) <- NULL
    colrangebyrow <- rangeby[!duplicated(rangeby[, 'row']),
                             c('row', 'col.low', 'col.high', 'x.low', 'x.high')]
    rowrangebycol <- rangeby[!duplicated(rangeby[, 'col']),
                             c('col', 'row.low', 'row.high', 'y.low', 'y.high')]

    tmp <- enlceil
    tmp$cardDirEW <- ifelse(tmp$col == 1, "west", ifelse(tmp$col == col.max, "east", "inner"))
    tmp$cardDirNS <- ifelse(tmp$row == 1, "south", ifelse(tmp$row == row.max, "north", "inner"))
    distinct_col <- tmp[!duplicated(tmp$col), c('col', 'x', 'strain', 'cardDirEW')]
    colnames(distinct_col) <- c('col', 'x', 'strainx', 'cardDirEW')
    distinct_row <- tmp[!duplicated(tmp$row), c('row', 'y', 'strain', 'cardDirNS')]
    colnames(distinct_row) <- c('row', 'y', 'strainy', 'cardDirNS')

    pnts <- merge(distinct_col, distinct_row, by = NULL)
    pnts <- pnts[order(pnts$col, pnts$row),]
    pnts$hpos <- ""
    for (row_idx in seq_len(nrow(pnts))) {
      row <- pnts[row_idx,]
      tmp <- colrangebyrow[colrangebyrow[, "row"] == row$row]
      pnts[row_idx, 'hpos'] <- ifelse(row$col >= tmp[2] & row$col <= tmp[3], 'on',
                                      ifelse(row$col < tmp[2], 'nw', 'se'))
    }
    rownames(pnts) <- NULL

    return(list(pnts = pnts,
                colrangebyrow = colrangebyrow,
                rowrangebycol = rowrangebycol,
                Mcol = col.max,
                Mrow = row.max))
  }

p_sv4Points <-
  function (xyP, wb) {
    # compute sv (raw + normalized (net)) for points

    mz <- wb$maze
    br <- wb$BRs$br # (possibly) inflated bounding rectangle
    rp <- wb$raster$pnts
    # no mixing with x and y in xyP
    colnames(rp) <- c('col', 'rx', 'strainx', 'cardDirEW', 'row', 'ry',
                      'strainy', 'cardDirNS', 'hpos', 'insol', 'inver')

    # We cannot rely on xyP having caseNo as a variable identifying a row.
    xyP <- cbind(pointID = paste0("point_", xyP$caseNo), xyP)
    xyOri <- xyP # keep (almost) original input data in xyO

    xyP <- p_sv4PointsSql(xyP, mz, rp, br)

    # names <- c('pointID', 'x', 'y', 'ezon', 'feas', 'idRE', 'idRW', 'insol', 'inver')
    names <- c('caseNo', 'pointID', 'x', 'y', 'insol', 'inver')
    xyQ <- xyP[, names]

    # add normalized (in-)solidity / (in-)veracity
    nces <- wb$nces

    xyQ$insol_net <- p_nciRaw2Norm(xyQ$insol, nces, solidity = TRUE)
    xyQ$inver_net <- p_nciRaw2Norm(xyQ$inver, nces, solidity = FALSE)
    xyQ$sol_net <- 1 - xyQ$insol_net
    xyQ$ver_net <- 1 - xyQ$inver_net

    xyR <- merge(x = xyOri, y = xyQ, by = "pointID", all.x = TRUE)
    xyR <- xyR[order(xyR$pointID),]

    res <- list(compacts = xyQ, specifics = xyR)

    return(res)
  }

p_sv4PointsSql <-
  function (xyP, mz, rp, br) {
    # TODO Check what's needed
    xyP$ezon <- FALSE
    xyP$feas <- FALSE
    xyP$ver_num <- NA
    xyP$ver_den <- NA
    xyP$sol_num <- NA
    xyP$sol_den <- NA
    xyP$ver_devi <- NA
    xyP$sol_devi <- NA
    xyP$insol <- NA
    xyP$inver <- NA
    xyP$nw <- FALSE
    xyP$se <- FALSE

    sv_db <- dbConnect(SQLite(), ":memory:")
    dbWriteTable(sv_db, "xyP", xyP)
    dbWriteTable(sv_db, "rp", rp)
    dbWriteTable(sv_db, "mz", mz)

    dbExecute(sv_db, CREATE_STANCE)
    dbExecute(sv_db, CREATE_E1)
    dbExecute(sv_db, UPDATE_XYP_1)
    dbExecute(sv_db, UPDATE_XYP_2,
              params = list(br1 = br[1], br2 = br[2], br3 = br[3], br4 = br[4]))
    dbExecute(sv_db, UPDATE_XYP_3, ,
              params = list(br1 = br[1], br2 = br[2], br3 = br[3], br4 = br[4]))
    dbExecute(sv_db, UPDATE_XYP_4)

    xyP <- dbReadTable(sv_db, "xyP")

    dbDisconnect(sv_db)

    return (xyP)
  }

###############################################################################
# HERE END
###############################################################################

p_make2SidedWebOrg <-
  function (ceilings, scope) {
    # compute on meshes formed by the extended ceiling's break-points
    # Meshes (maze) form a partition of the bounding rectangle based on the breakpoints
    # in the extended ceiling.

    # Note: enlceil is a dataframe, not an matrix (mixed types)
    enlceil <- cbind(data.frame(ceilings$xceil), strain = "regular")

    # if enlargement then add points to xceil to enlarge it and obtain enlceil
    enlarged <- FALSE
    if (p_ENLARGEMENT_CONSTANT > 0) {
      enlarged <- TRUE
      dx <- scope[2] - scope[1]
      dy <- scope[4] - scope[3]
      extra.row <- data.frame(x = scope[1] - p_ENLARGEMENT_CONSTANT * dx / 2,
                              y = scope[3] - p_ENLARGEMENT_CONSTANT * dy / 2,
                              col = 0, row = 0, strain = "enlarged")
      enlceil <- rbind(extra.row, enlceil)


      tmp <- tail(enlceil, n = 1)
      extra.row <- data.frame(x = scope[2] + p_ENLARGEMENT_CONSTANT * dx / 2,
                              y = scope[4] + p_ENLARGEMENT_CONSTANT * dy / 2,
                              col = tmp[3] + 1, row = tmp[4] + 1, strain = "enlarged")
      enlceil <- rbind(enlceil, extra.row)
      enlceil[, 3:4] <- enlceil[, 3:4] + 1
      row.names(enlceil) <- NULL
    }

    ceilings$enlceil <- enlceil
    # xy (and col-row) raster with info
    ras <- p_twoSidedRasterOrg(enlceil)
    rp <- ras$pnts

    # TODO Rename to scope.enl
    # bounding rectangle (enlarged) coordinates from xyCR (= xceil):
    br <- c(min(rp['x']), max(rp['x']), min(rp['y']), max(rp['y']))

    # TODO Rename to scopes
    BRs <- list(obr = scope, br = br)

    # Out of the raster points we build the mesh (= partition by mazes = rectangles)
    # Meshes: raster points (x, y) are considered as south-east points in a mesh
    # below procedure may give at first some incomplete/degenerate/non-existing
    # nw points/meshes which subsequently get removed
    colnames(rp) <- c('col', 'se_x', 'strainx', 'cardDirEW',
                      'row', 'se_y', 'strainy', 'cardDirNS', 'se_pos')

    # Now compute the meshes with current points in rp considered as se-points.
    r1 <- rp
    r1 <- cbind(r1, rp[, 'row'] - 1, rp[, 'col'] + 1)
    colnames(r1) <- c('col.y', 'nw_x', 'strainx', 'cardDirEW',
                      'row.y', 'nw_y', 'strainy', 'cardDirNS',
                      'nw_pos', 'row', 'col')

    mesh <- merge(rp, r1, by = c("row", "col"))
    # Remove "invalid" meshes - having corners or row-col with NA-values:
    mesh <- mesh[complete.cases(mesh[, c("row.y", "col.y", "col", "row", "nw_x")]),]
    mesh <- mesh[order(mesh$col),]

    # fixing up remaining, valid, meshes, such as
    # using only the 4 ordinal directions (e, w, s, n) for the mesh:
    maze <- cbind(mesh['col'], mesh['se_x'], mesh['strainx.x'], mesh['cardDirEW.x'],
                  mesh['row'], mesh['se_y'], mesh['strainy.x'], mesh['cardDirNS.x'], mesh['se_pos'],
                  mesh['col.y'], mesh['nw_x'], mesh['strainx.y'], mesh['cardDirEW.y'],
                  mesh['row.y'], mesh['nw_y'], mesh['strainy.y'], mesh['cardDirNS.y'], mesh['nw_pos'])
    colnames(maze) <- c('e_col', 'e_x', 'strainx.x', 'cardDirEW.x',
                        's_row', 's_y', 'strainy.x', 'cardDirNS.x', 'se_pos',
                        'w_col', 'w_x', 'strainx.y', 'cardDirEW.y',
                        'n_row', 'n_y', 'strainy.y', 'cardDirNS.y', 'nw_pos')
    maze$onEdge <- 'enlarged' == maze['strainx.x'] |
      'enlarged' == maze['strainx.y'] |
      'enlarged' == maze['strainy.x'] |
      'enlarged' == maze['strainy.y']
    maze[c('strainx.x', 'strainx.y', 'strainy.x', 'strainy.y')] <- NULL

    # Note. Points (e_x, s_y) are south and east lines in their maze (cell)
    # and (w_x, n_y) are the north and west lines in that maze, and so on.
    # Note that we only track positions on the nw and se intersection points.
    # Such is sufficient (for our purposes) as the ceiling runs sw-ne.
    # w_x ~ x.low; e_x ~ x.high; s_y ~y.low; n_y ~ y.high
    # some additional info on mazes for easy identification:
    maze <- maze[with(maze, order(maze$e_x, maze$s_y)),]
    maze <- cbind(mazeID = paste0('maze_', seq.int(nrow(maze))), maze)

    # position (stance), south-east or north-west, or bisected, of meshes wrt ceiling:
    maze$stance <- as.vector(
      ifelse(maze['nw_pos'] == "nw" & maze['se_pos'] == "se", "bisect",
             ifelse(maze['nw_pos'] == "on" | maze['nw_pos'] == "se",
                    "se", "nw")))

    # Areas of meshes (with bi-sect counting for half):
    maze$area <- as.vector(
      ifelse(maze$stance == "bisect",
             (maze[['e_x']] - maze[['w_x']]) * (maze[['n_y']] - maze[['s_y']]) / 2,
             (maze[['e_x']] - maze[['w_x']]) * (maze[['n_y']] - maze[['s_y']])))
    # Do away with irrelevant 0-area meshes, probably redundant, but ... safety first:
    maze <- maze[maze['area'] > 0,]

    # Unconditional areas to north-west (for each of the 4 corners of a mesh)
    # Note. The same point, when a corner in 4 meshes, gets calculated 4 times
    maze$se_NW_den <- unlist((maze['e_x'] - br[1]) * (br[4] - maze['s_y']))
    maze$nw_NW_den <- unlist((maze['w_x'] - br[1]) * (br[4] - maze['n_y']))
    maze$sw_NW_den <- unlist((maze['w_x'] - br[1]) * (br[4] - maze['s_y']))
    maze$ne_NW_den <- unlist((maze['e_x'] - br[1]) * (br[4] - maze['n_y']))

    # and total (unconditional) area to the south-east of each point in a mesh:
    maze$se_SE_den <- unlist((br[2] - maze['e_x']) * (maze['s_y'] - br[3]))
    maze$nw_SE_den <- unlist((br[2] - maze['w_x']) * (maze['n_y'] - br[3]))
    maze$sw_SE_den <- unlist((br[2] - maze['w_x']) * (maze['s_y'] - br[3]))
    maze$ne_SE_den <- unlist((br[2] - maze['e_x']) * (maze['n_y'] - br[3]))

    # Now for in-solidity numerators (num) for each of the 4 corners of a mesh:
    maze <- cbind(maze, se_NW_num = NA, nw_NW_num = NA, sw_NW_num = NA, ne_NW_num = NA)

    # Deviance areas to north-west (for each of the 4 corners of a mesh)
    # Deviance = from what is expected for a point on the ceiling
    # (here the feasible area is to the NW of the point under consideration)
    # Note: inefficiency of a point may get (re-)calculated up to 4 times
    for (i in seq_len(nrow(maze))) { # stance in "se, bisect": 100%/50% in feas. space
      cl <- maze$e_col[i]; rw <- maze$s_row[i]
      tmp <- maze[maze['e_col'] <= cl &
                    maze['s_row'] >= rw &
                    (maze['stance'] == "bisect" | maze['stance'] == "se"),]
      maze$se_NW_num[i] <- sum(tmp$area)

      cl <- maze$w_col[i]; rw <- maze$n_row[i]
      tmp <- maze[maze['e_col'] <= cl &
                    maze['s_row'] >= rw &
                    (maze['stance'] == "bisect" | maze['stance'] == "se"),]
      maze$nw_NW_num[i] <- sum(tmp$area)

      rw <- maze$s_row[i]; cl <- maze$w_col[i]
      tmp <- maze[maze['e_col'] <= cl &
                    maze['s_row'] >= rw &
                    (maze['stance'] == "bisect" | maze['stance'] == "se"),]
      maze$sw_NW_num[i] <- sum(tmp$area)


      rw <- maze$n_row[i]; cl <- maze$e_col[i]
      tmp <- maze[maze['e_col'] <= cl &
                    maze['s_row'] >= rw &
                    (maze['stance'] == "bisect" | maze['stance'] == "se"),]
      maze$ne_NW_num[i] <- sum(tmp$area)
    }

    # # Now then for in-veracity numerators for each of the 4 corners of a maze:
    maze <- cbind(maze, se_SE_num = NA, nw_SE_num = NA, sw_SE_num = NA, ne_SE_num = NA)

    # Deviance areas to south-east (for each of the 4 corners of a maze)
    # Deviance from what is expected for a point on the ceiling
    # (here the feasible area is to the SE)
    # inefficient calculation: a point/value may get (re-)calculated up to 4 times
    for (i in seq_len(nrow(maze))) {
      rw <- maze$s_row[i]; cl <- maze$e_col[i]
      tmp <- maze[maze['w_col'] >= cl &
                    maze['n_row'] <= rw &
                    (maze['stance'] == "bisect" | maze['stance'] == "nw"),]
      maze$se_SE_num[i] <- sum(tmp$area)

      rw <- maze$n_row[i]; cl <- maze$w_col[i]
      tmp <- maze[maze['w_col'] >= cl &
                    maze['n_row'] <= rw &
                    (maze['stance'] == "bisect" | maze['stance'] == "nw"),]
      maze$nw_SE_num[i] <- sum(tmp$area)

      rw <- maze$s_row[i]; cl <- maze$w_col[i]
      tmp <- maze[maze['w_col'] >= cl &
                    maze['n_row'] <= rw &
                    (maze['stance'] == "bisect" | maze['stance'] == "nw"),]
      maze$sw_SE_num[i] <- sum(tmp$area)

      rw <- maze$n_row[i]; cl <- maze$e_col[i]
      tmp <- maze[maze['w_col'] >= cl &
                    maze['n_row'] <= rw &
                    (maze['stance'] == "bisect" | maze['stance'] == "nw"),]
      maze$ne_SE_num[i] <- sum(tmp$area)
    }

    # Finished with areas, now for the deviance calculations.
    # In-solidity and in-veracity ratios: circumvent division by zero.
    # As an in-solidity involves the north&west of the br, special care
    # (to avoid cancellation/division by zero) is needed for meshes
    # touching (osculating) the west- or north-side of the br
    maze$nw_loc <- as.character(
      ifelse((maze['w_x'] == br[1]) & (maze['n_y'] == br[4]), "north-west",
             ifelse(maze['w_x'] == br[1], "west",
                    ifelse(maze['n_y'] == br[4], "north", "other"))))
    maze$se_loc <- as.character(
      ifelse((maze['e_x'] == br[2]) & (maze['s_y'] == br[3]), "south-east",
             ifelse(maze['e_x'] == br[2], "east",
                    ifelse(maze['s_y'] == br[3], "south", "other"))))

    # vx: west-east x-value of up-going (going north(-east) ~ vertical) ceiling
    # per mesh, id-ed thru south (se-sw) row: vx = x where the ceiling crosses to ne.
    lookup_vx <- cbind(s_row = ras$colrangebyrow[, 'row'],
                       vx = ras$colrangebyrow[, 'x.high'],
                       vc = ras$colrangebyrow[, 'col.high'])
    maze <- merge(maze, lookup_vx, by = "s_row", keep = NULL)

    # the point (s_row,vc) is where the ceiling crosses the line determined
    # by the south of the mesh towards the line determined by the north of the mesh,
    # in particular (se_row,vc) is on the ceiling.

    # we need the slope for the ceiling running from the south-line to the north-line of mesh
    # if the point (se_row,vc) is on the east of the bb the slope, vs, is infinite,
    # meaning the slope is vertical, otherwise (se_row,vc) is the sw-point of a mesh in which
    # the ceiling can be assumed to run for the south-line to the north-line.
    # Search for the mesh with sw-corner (vc, se_row = sw_row):
    lookup_maze <- cbind(maze['w_col'],
                         maze['w_x'], maze['s_row'], maze['s_y'], maze['e_col'],
                         maze['e_x'], maze['n_row'], maze['n_y'], maze['stance'])
    lookup_maze <- lookup_maze[with(lookup_maze, order(lookup_maze$w_col)),]
    colnames(lookup_maze) <- c('vc', 'w_x.v', 's_row', 's_y.v', 'e_col.v',
                               'e_x.v', 'n_row.v', 'n_y.v', 'stance.v')
    xmaze <- merge(x = maze, y = lookup_maze, by = c('vc', 's_row'), all.x = TRUE)
    xmaze <- xmaze[with(xmaze, order(xmaze$e_col, xmaze$w_col)),]

    for (i in seq_len(nrow(xmaze))) {
      row <- xmaze[i,]
      tmp <- ifelse(row['stance.v'] == "bisect",
                    (row['n_y.v'] - row['s_y.v']) / (row['e_x.v'] - row['w_x.v']),
                    ifelse(is.na(row['stance.v']), Inf, Inf))
      xmaze$vslope[i] <- as.numeric(tmp)
    }
    # cleaning up superfluous values (for efficiency):
    xmaze[, colnames(xmaze)[grepl(".*\\.v", colnames(xmaze))]] <- NULL

    # Now, the analogous computation in the y-direction:
    # horizontal ceiling in south-north (hy) of maze:
    # Note. We will use south-west [] as (join_by) reference point.
    lookup_hy <- cbind(w_col = ras$rowrangebycol[, 'col'],
                       hy = ras$rowrangebycol[, 'y.high'],
                       hr = ras$rowrangebycol[, 'row.high'])
    xmaze <- merge(x = xmaze, y = lookup_hy, by = "w_col",
                   keep = NULL, all.x = TRUE)

    colnames(lookup_maze) <- c('w_col', 'w_x.h', 'hr', 's_y.h', 'e_col.h',
                               'e_x.h', 'n_row.h', 'n_y.h', 'stance.h')
    xymaze <- merge(xmaze, lookup_maze, by = c("hr", "w_col"), keep = NULL, all.x = TRUE)

    for (i in seq_len(nrow(xymaze))) {
      row <- xymaze[i,]
      tmp <- ifelse(row['stance.h'] == "bisect",
                    (row['n_y.h'] - row['s_y.h']) / (row['e_x.h'] - row['w_x.h']),
                    ifelse(is.na(row['stance.h']), 0, 0))
      xymaze$hslope[i] <- as.numeric(tmp)
    }
    # cleaning up superfluous values (for efficiency):
    xymaze[, colnames(xymaze)[grepl(".*\\.h", colnames(xymaze))]] <- NULL

    # Note. Computing (in-)solidity: looking for feasible space to nw.
    # So, of special concern are meshes touching the bb on the north and/or west.
    # Note: when !(stance %in% c("bisect", "nw") it has to be "se".
    # So, fall-through cases stance %in% c("bisect", "nw") are "se" cases.
    for (i in seq_len(nrow(xymaze))) {
      row <- xymaze[i,]

      sw_insol <- ifelse(row['stance'] %in% c("bisect", "nw"), 0,
                         ifelse(row['nw_loc'] == "west",
                                (row['hy'] - row['s_y']) / (br[4] - row['s_y']),
                                row['sw_NW_num'] / row['sw_NW_den']))
      xymaze$sw_insol[i] <- as.numeric(sw_insol)

      ne_insol <- ifelse(row['stance'] %in% c("bisect", "nw"), 0,
                         ifelse(row['nw_loc'] == "north",
                                (row['e_x'] - row['w_x']) / (row['e_x'] - br[1]),
                                row['ne_NW_num'] / row['ne_NW_den']))
      xymaze$ne_insol[i] <- as.numeric(ne_insol)

      nw_insol <- ifelse(row['stance'] %in% c("bisect", "nw") | (row['nw_NW_den'] == 0),
                         0,
                         row['nw_NW_num'] / row['nw_NW_den'])
      xymaze$nw_insol[i] <- as.numeric(nw_insol)

      xymaze$se_insol[i] <- as.numeric(row['se_NW_num'] / row['se_NW_den'])
    }

    for (i in seq_len(nrow(xymaze))) {
      row <- xymaze[i,]

      sw_inver <- ifelse(row['stance'] %in% c("bisect", "se"), 0,
                         ifelse(row['se_loc'] == "south",
                                (row['e_x'] - row['w_x']) / (br[2] - row['w_x']),
                                row['sw_SE_num'] / row['sw_SE_den']))
      xymaze$sw_inver[i] <- as.numeric(sw_inver)

      ne_inver <- ifelse(row['stance'] %in% c("bisect", "se"), 0,
                         ifelse(row['se_loc'] == "east",
                                (row['n_y'] - row['s_y']) / (row['n_y'] - br[3]),
                                row['ne_SE_num'] / row['ne_SE_den']))
      xymaze$ne_inver[i] <- as.numeric(ne_inver)

      xymaze$nw_inver[i] <- as.numeric(row['nw_SE_num'] / row['nw_SE_den'])

      se_inver <- ifelse(
        row['stance'] %in% c("bisect", "se") | row['se_SE_den'] == 0,
        0, row['se_SE_num'] / row['se_SE_den'])
      xymaze$se_inver[i] <- as.numeric(se_inver)
    }

    xymaze$se_inacc <- pmax(xymaze$se_insol, xymaze$se_inver)
    xymaze$sw_inacc <- pmax(xymaze$sw_insol, xymaze$sw_inver)
    xymaze$nw_inacc <- pmax(xymaze$nw_insol, xymaze$nw_inver)
    xymaze$ne_inacc <- pmax(xymaze$ne_insol, xymaze$ne_inver)

    ras$pnts <- p_doInsolInverLookUpOrg(ras$pnts, xymaze)

    # effect size:
    se_maze <- xymaze[xymaze['s_row'] == min(xymaze['s_row']) &
                        xymaze['e_col'] == max(xymaze['e_col']),]
    nces <- 1 - se_maze$se_insol

    dossier <- list(message = "", portrayal = "")
    if (nrow(xymaze) == 1) {
      if (xymaze$stance != "bisect") { # ceiling fully in bb boundary
        if (xymaze$stance == "nw") {
          TM1 <- "\n Single mesh: Empty space goes to south-east of bounding rectangle! \n"
          TM2 <- "There is no feasible space: in-veracity equals 1 (except on ceiling). \n"
          TM3 <- "Any iso-(in-)veracity-line will be meaningless! \n \n"
          dossier$message <- paste0(TM1, TM2, TM3)
          dossier$portrayal <- "no_feasible_space"
        } else { # xymaze$stance == "se"
          TM1 <- "\n Single mesh: Feasible point at the north-west of bounding rectangle! \n"
          TM2 <- "There is no empty space and in-solidity equals 1 (except on ceiling). \n"
          TM3 <- "Any iso-(in-)solidity-line will be meaningless! \n \n"
          dossier$message <- paste0(TM1, TM2, TM3)
          dossier$portrayal <- "no_empty_space"
        }
      }
    }

    xymaze$nesw_sqr <- ifelse(
      xymaze$w_x == br[1] &
        xymaze$s_y == br[3] &
        xymaze$stance != "bisect",
      "sw_sqr",
      ifelse(
        xymaze$e_x == br[2] &
          xymaze$n_y == br[4] &
          xymaze$stance != "bisect",
        "ne_sqr",
        "other"))

    return(list(ceilings = ceilings, raster = ras, BRs = BRs, enlarged = enlarged,
                maze = xymaze, nces = nces, dossier = dossier))
  }

p_twoSidedRasterOrg <-
  function (enlceil) {
    # raster from xy-column-rows (later the meshes)
    # TODO Rename everything

    col.max <- max(enlceil['col'])
    row.max <- max(enlceil['row'])

    colx <- enlceil[c('col', 'x', 'strain')]
    distinct_col <- NULL
    for (i in seq_len(nrow(colx))) {
      row <- colx[i,]
      cardDirEW <- ifelse(row[1] == 1, "west", ifelse(row[1] == col.max, "east", "inner"))
      if (!(row[1] %in% distinct_col[, 1])) {
        distinct_col <- rbind(distinct_col, cbind(row, cardDirEW = cardDirEW))
      }
    }
    colnames(distinct_col) <- c('col', 'x', 'strainx', 'cardDirEW')

    rowy <- enlceil[c('row', 'y', 'strain')]
    distinct_row <- NULL
    for (i in seq_len(nrow(rowy))) {
      row <- rowy[i,]
      cardDirNS <- ifelse(row[1] == 1, "south", ifelse(row[1] == row.max, "north", "inner"))
      if (!(row[1] %in% distinct_row[, 1])) {
        distinct_row <- rbind(distinct_row, cbind(row, cardDirNS))
      }
    }
    colnames(distinct_row) <- c('row', 'y', 'strainy', 'cardDirNS')

    pnts <- NULL
    for (i in seq_len(nrow(distinct_col))) {
      for (j in seq_len(nrow(distinct_row))) {
        new.row <- cbind(distinct_col[i,], distinct_row[j,])
        pnts <- rbind(pnts, new.row)
      }
    }

    mincol <- pnts[pnts['col'] == 1,]
    colnames(mincol) <- paste0('m', colnames(mincol))
    maxcol <- pnts[pnts['col'] == max(pnts['col']),]
    colnames(maxcol) <- paste0('M', colnames(maxcol))
    names(maxcol)[names(maxcol) == 'Mrow'] <- 'mrow'
    horiz <- merge(mincol, maxcol, by = "mrow")

    minrow <- pnts[pnts['row'] == 1,]
    colnames(minrow) <- paste0('m', colnames(minrow))
    maxrow <- pnts[pnts['row'] == max(pnts['row']),]
    colnames(maxrow) <- paste0('M', colnames(maxrow))
    names(maxrow)[names(maxrow) == 'Mcol'] <- 'mcol'
    verti <- merge(minrow, maxrow, by = "mcol")
    # Note. horiz and verti will only be used for producing a grid in plots.

    colrangebyrow <- NULL
    for (row in seq_along(unique(enlceil[, 'row']))) {
      rows <- enlceil[enlceil['row'] == row,]
      tmp <- c(row, min(rows['col']), max(rows['col']), min(rows['x']), max(rows['x']))
      colrangebyrow <- rbind(colrangebyrow, tmp)
    }
    rownames(colrangebyrow) <- NULL
    colnames(colrangebyrow) <- c('row', 'col.low', 'col.high', 'x.low', 'x.high')

    rowrangebycol <- NULL
    for (col in seq_along(unique(enlceil[, 'col']))) {
      cols <- enlceil[enlceil['col'] == col,]
      # TODO Why is the order different?
      tmp <- c(col, min(cols['y']), max(cols['y']), min(cols['row']), max(cols['row']))
      rowrangebycol <- rbind(rowrangebycol, tmp)
    }
    rownames(rowrangebycol) <- NULL
    colnames(rowrangebycol) <- c('col', 'y.low', 'y.high', 'row.low', 'row.high')

    col.new <- NULL
    for (i in seq_len(nrow(pnts))) {
      col <- pnts[i,][['col']]
      row <- pnts[i,][['row']]
      low <- colrangebyrow[colrangebyrow[, "row"] == row][2]
      high <- colrangebyrow[colrangebyrow[, "row"] == row][3]
      tmp <- ifelse(col >= low & col <= high, 'on', ifelse(col < low, 'nw', 'se'))
      col.new <- c(col.new, tmp)
    }
    pnts$hpos <- col.new
    # TODO
    rownames(pnts) <- NULL

    return(list(pnts = pnts,
                colrangebyrow = colrangebyrow,
                rowrangebycol = rowrangebycol,
                horiz = horiz,
                verti = verti,
                Mcol = col.max,
                Mrow = row.max))
  }

p_doInsolInverLookUpOrg <-
  function (pnts, xymaze) {
    pnts$insol <- NULL
    pnts$inver <- NULL
    for (i in seq_len(nrow(pnts))) {
      row <- pnts[i,]

      if (row['cardDirEW'] != "east" & row['cardDirNS'] != "north") {
        tmp <- xymaze[xymaze['w_x'] == row[['x']] & xymaze['s_y'] == row[['y']],]
        pnts$insol[i] <- tmp$sw_insol
        pnts$inver[i] <- tmp$sw_inver
      }
      else if (row['cardDirEW'] != "east" & row['cardDirNS'] == "north") {
        tmp <- xymaze[xymaze['w_x'] == row[['x']] & xymaze['n_y'] == row[['y']],]
        pnts$insol[i] <- tmp$nw_insol
        pnts$inver[i] <- tmp$nw_inver
      }
      else if (row['cardDirEW'] == "east" & row['cardDirNS'] != "south") {
        tmp <- xymaze[xymaze['e_x'] == row[['x']] & xymaze['n_y'] == row[['y']],]
        pnts$insol[i] <- tmp$ne_insol
        pnts$inver[i] <- tmp$ne_inver
      }
      else if (row['cardDirEW'] == "east" & row['cardDirNS'] == "south") {
        tmp <- xymaze[xymaze['e_x'] == row[['x']] & xymaze['s_y'] == row[['y']],]
        pnts$insol[i] <- tmp$se_insol
        pnts$inver[i] <- tmp$se_inver
      }
      else {
        pnts$insol[i] <- NA
        pnts$inver[i] <- NA
      }
    }

    return(pnts)
  }

p_sv4PointsOrg <-
  function (xyP, wb) {
    # compute sv (raw + normalized (net)) for points

    mz <- wb$maze
    br <- wb$BRs$br # (possibly) inflated bounding rectangle
    rp <- wb$raster$pnts
    # no mixing with x and y in xyP
    colnames(rp) <- c('col', 'rx', 'strainx', 'cardDirEW', 'row', 'ry',
                      'strainy', 'cardDirNS', 'hpos', 'insol', 'inver')

    # We cannot rely on xyP having caseNo as a variable identifying a row.
    xyP <- cbind(pointID = paste0("point_", xyP$caseNo), xyP)
    xyOri <- xyP # keep (almost) original input data in xyO

    for (i in seq_len(nrow(xyP))) {
      tmp <- p_soveLookupCompsOrg(xyP[i,], rp, mz, br)
      #xyP$idRE[i] <- tmp$idRectE
      #xyP$idRW[i] <- tmp$idRectW
      #xyP$ezon[i] <- tmp$ezon
      #xyP$feas[i] <- tmp$feas
      xyP$insol[i] <- tmp$insol
      xyP$inver[i] <- tmp$inver
    }

    # names <- c('pointID', 'x', 'y', 'ezon', 'feas', 'idRE', 'idRW', 'insol', 'inver')
    names <- c('caseNo', 'pointID', 'x', 'y', 'insol', 'inver')
    xyQ <- xyP[, names]

    # add normalized (in-)solidity / (in-)veracity
    nces <- wb$nces

    xyQ$insol_net <- p_nciRaw2Norm(xyQ$insol, nces, solidity = TRUE)
    xyQ$inver_net <- p_nciRaw2Norm(xyQ$inver, nces, solidity = FALSE)
    xyQ$sol_net <- 1 - xyQ$insol_net
    xyQ$ver_net <- 1 - xyQ$inver_net

    xyR <- merge(x = xyOri, y = xyQ, by = "pointID", all.x = TRUE)
    xyR <- xyR[order(xyR$pointID),]

    res <- list(compacts = xyQ, specifics = xyR)

    return(res)
  }

p_soveLookupCompsOrg <-
  function (row, rp, mz, br) { # sove by lookup and comps
    x <- unlist(row['x'])
    y <- unlist(row['y'])

    xyS <- mz[x >= mz$w_x &
                x <= mz$e_x &
                y >= mz$s_y &
                y <= mz$n_y,]
    xyS <- xyS[order(xyS$e_x, xyS$s_y),]

    Stands <- xyS$stance
    nw <- is.element("nw", Stands)
    se <- is.element("se", Stands)

    # TODO
    # E1 <- slice(xyS[order(-xyS$e_x, xyS$s_y),], 1)
    E1 <- head(xyS, 1)

    feas <- ifelse(
      se, TRUE, ifelse(
        E1$stance == "bisect",
        (y - E1['s_y']) * (E1['e_x'] - E1['w_x']) <= (x - E1['w_x']) * (E1['n_y'] - E1['s_y']),
        FALSE))

    ezon <- ifelse(
      nw, TRUE, ifelse(
        E1$stance == "bisect",
        (y - E1['s_y']) * (E1['e_x'] - E1['w_x']) >= (x - E1['w_x']) * (E1['n_y'] - E1['s_y']),
        FALSE))

    insol <- inver <- NA
    luRP <- rp[rp['rx'] == x & rp['ry'] == y,]
    if (nrow(luRP) > 0) {
      inver <- luRP$inver
      insol <- luRP$insol
    }
    inver <- ifelse(
      !is.na(inver), inver, ifelse(
        feas, 0,
        p_deviOrg(x, y, E1['w_x'], E1['s_y'], E1['vx'], E1['hy'], E1['vslope'],
                  E1['hslope'], E1['sw_SE_num'], E1['sw_SE_den'], E1['n_y'],
                  E1['e_x'], br, E1['nesw_sqr'], ilk = "inver")))

    insol <- ifelse(
      !is.na(insol), insol, ifelse(
        ezon, 0,
        p_deviOrg(x, y, E1['w_x'], E1['s_y'], E1['vx'], E1['hy'], E1['vslope'],
                  E1['hslope'], E1['sw_NW_num'], E1['sw_NW_den'], E1['n_y'],
                  E1['e_x'], br, E1['nesw_sqr'], ilk = "insol")))

    return(list(
      inver = inver, insol = insol

      # TODO Not needed, just for development
      # , idRectE = E1$mazeID, idRectW = E1$mazeID,
      # feas = feas, ezon = ezon
    ))
  }

p_deviOrg <-
  function (x, y, x0, y0, vx, hy, sv, sh, t0, s0, ny, ex, br, sqr, ilk = "insol") {
    dev <- 0
    dx <- x - x0; dy <- y - y0 # local coordinates
    if (ilk == "inver") { # ilk = inver
      xx <- br[2]; yy <- br[3] # reference values
      # east - in-veracity, in the in-feasible space
      # y != yy unless we deal with a trivial case
      if (x == xx) {
        dev <- (y - hy) / (y - yy)
      }
      # south - in-veracity, in the in-feasible space
      # x != xx unless we deal with a trivial case
      if (y == yy) {
        dev <- (vx - x) / (xx - x)
      }
      if ((x != xx) & (y != yy)) {
        nd <- p_numdenOrg(x, y, x0, y0, vx, hy, sv, sh, t0, s0, xx, yy)
        dev <- ifelse(
          (sqr == "ne_sqr") & (nd$den == 0),
          dy / (y - yy),
          ifelse((sqr == "sw_sqr") & (nd$den == 0),
                 (ex - x) / (br[2] - x),
                 nd$num / nd$den))
      } # ilk = inver
    } else { # ilk = "insol"
      xx <- br[1]; yy <- br[4] # reference values
      # west - in-solidity, in the feasible space
      # y != yy unless we deal with a trivial case
      if (x == xx) {
        dev <- (y - hy) / (y - yy)
      }
      # north - in-solidity, in the feasible space
      # x != xx unless we deal with a trivial case
      if (y == yy) {
        dev <- (x - vx) / (xx - x)
      }
      if ((x != xx) & (y != yy)) {
        nd <- p_numdenOrg(x, y, x0, y0, vx, hy, sv, sh, t0, s0, xx, yy)
        dev <- ifelse(
          (sqr == "ne_sqr") & (nd$den == 0),
          dx / (x - xx),
          ifelse(
            (sqr == "sw_sqr") & (nd$den == 0),
            (ny - y) / (br[4] - y),
            nd$num / nd$den))
      } # ilk = "insol"
    }
    return(unlist(dev))
  }

p_numdenOrg <-
  function (x, y, x0, y0, vx, hy, sv, sh, t0, s0, xx, yy) {
    # num and den: ratio = num/den
    # At mesh with sw-point = (x0,y0), in local coordinates (dx = x - x0, dy = y - y0),
    # numerator = dx^2 sh/2 - dx dy + dy^2/(2 sv) + dx (hy - y0) - dy (x0 - vx) + t0
    # denominator = - dx dy + dx (ym -y0)- dy (x0 - vx) + s0
    # For iso this leads to a conic section ax^2 + 2nxy + by^2 + 2hx + 2ky + c = 0
    # reading (for in-solidity)
    # dx^2 sh/2 - (1-JJ) dx dy + dy^2/(2 sv) + dx (hy - y0  - JJ (hy -yM)) -
    #     dy (x - vx - JJ (x - xm)) + t0 - JJ s0 = 0
    # where t0 and s0 are numerator and denominator of
    # the (in-solidity) inaccuracy measure at the sw-point (x0,y0).
    dx <- x - x0; dy <- y - y0 # local coordinates
    num <- dx^2 * sh / 2 - dx * dy +
      dy^2 / (2 * sv) +
      dx * (hy - y0) - dy * (x0 - vx) + t0
    den <- -dx * dy + dx * (yy - y0) - dy * (x0 - xx) + s0
    return(list(num = num, den = den))
  }

###############################################################################
# TODO Remove above
###############################################################################

p_nciRaw2Norm <-
  function (riacc, d, solidity = TRUE) {
    # riacc = raw in-accuracy (in-solidity or in-veracity) measure
    # d = effect size; solidity = TRUE: solidity transformation

    # racc is some (solidity/veracity) raw measure
    racc <- 1 - unlist(riacc)

    if (solidity) {
      nsol <- (racc - d) / (1 - d) # nrm solidity
      return(1 - nsol)             # nrm in-solidity = (1-racc)/(1-d)
    } else {
      nver <- (racc - (1 - d)) / d # nrm veracity
      return(1 - nver)             # nrm in-veracity)
    }
  }

p_svGauge <-
  function (svP, critLevs = c(cris = .2, criv = .1)) {
    stopifnot(all(c("insol_net", "inver_net") %in% names(svP)))

    # Flags
    satS <- svP$insol_net == 0
    medS <- 0 <= svP$insol_net &
      svP$insol_net <= critLevs[["cris"]] &
      !satS
    lowS <- !medS & !satS

    satP <- svP$inver_net == 0
    medP <- 0 <= svP$inver_net &
      svP$inver_net <= critLevs[["criv"]] &
      !satP
    lowP <- !medP & !satP

    # NA-safe combos
    satD <- ifelse(is.na(satS), FALSE, satS) & ifelse(is.na(satP), FALSE, satP)
    medSsatD <- ifelse(is.na(medS), FALSE, medS) | satD

    cnt <- function (x) sum(ifelse(is.na(x), FALSE, x))
    gauge <- list(
      satD = cnt(satD),
      medS = cnt(medS),
      medP = cnt(medP),
      lowS = cnt(lowS),
      lowP = cnt(lowP),
      satS = cnt(satS),
      satP = cnt(satP),
      medSsatD = cnt(medSsatD)
    )

    # Sharpness
    denom0 <- gauge$satD + gauge$medS + gauge$medP
    sharp0 <- if (denom0 != 0) (gauge$satD + gauge$medS - gauge$medP) / denom0 else NA_real_
    denom1 <- gauge$medS + gauge$medP
    sharp1 <- if (denom1 != 0) (gauge$medS) / denom1 else NA_real_

    # Percentages & totals
    numC <- nrow(svP)
    gauge$numC <- numC
    gauge$medSpct <- if (numC > 0) 100 * gauge$medS / numC else NA_real_
    gauge$medPpct <- if (numC > 0) 100 * gauge$medP / numC else NA_real_
    gauge$medSsatDpct <- if (numC > 0) 100 * gauge$medSsatD / numC else NA_real_
    gauge$nribbon <- gauge$medSsatD + gauge$medP # number of points in ribbon
    gauge$sharp0 <- sharp0
    gauge$sharp1 <- sharp1

    # Return flags attached to svP (useful downstream)
    svP$satS <- satS
    svP$medS <- medS
    svP$lowS <- lowS
    svP$satP <- satP
    svP$medP <- medP
    svP$lowP <- lowP
    svP$satD <- satD
    svP$medSsatD <- medSsatD

    list(svP = svP, gauge = gauge)
  }

p_ensure_pct_cols <-
  function (gauge, n_points = NULL) {
    if ((!("numC" %in% names(gauge)) || is.na(gauge$numC)) && !is.null(n_points)) {
      gauge$numC <- n_points
    }
    if (is.na(gauge$numC) || gauge$numC <= 0) {
      return(gauge)
    }
    if (!("medSpct" %in% names(gauge)) && "medS" %in% names(gauge)) {
      gauge$medSpct <- 100 * gauge$medS / gauge$numC
    }
    if (!("medSsatDpct" %in% names(gauge)) && "medSsatD" %in% names(gauge)) {
      gauge$medSsatDpct <- 100 * gauge$medSsatD / gauge$numC
    }
    if (!("medPpct" %in% names(gauge)) && "medP" %in% names(gauge)) {
      gauge$medPpct <- 100 * gauge$medP / gauge$numC
    }
    return(gauge)
  }

p_get_nca_metrics <-
  function (df, condition, outcome, ceiling, test_rep = 1000) {
    model <- nca_analysis(df, condition, outcome, ceilings = ceiling, test.rep = test_rep)
    part <- function (param) nca_extract(model, x = condition, ceiling = ceiling, param = param)
    list(
      effect_size = part(param = 'Effect size'),
      p_value = part(param = 'p-value'),
      fit = part(param = 'Fit'),
      c_accuracy = part(param = 'Ceiling accuracy'),
      npeers = nrow(model$peers[[ceiling]][[condition]])
    )
  }

p_compute_spread <-
  function (svP_flags, web) {
    # x-range from the original BR (outer bounding range)
    br <- web$BRs$obr
    xmin <- br[1]
    xmax <- br[2]

    # x for rows in the ribbon: medS U satD  (NA-safe)
    in_ribbon <- ifelse(is.na(svP_flags$medSsatD), FALSE, svP_flags$medSsatD)
    x_med <- svP_flags$x[in_ribbon]

    if (length(x_med) > 0L) {
      return(p_spread_index(x_med, xmin, xmax))
    } else {
      return(NA_real_)
    }
  }

p_spread_index <-
  function (x, xmin, xmax) {
    stopifnot(is.numeric(x))
    if (!is.finite(xmin) ||
      !is.finite(xmax) ||
      xmax <= xmin) {
      stop("xmax must be > xmin, both finite.")
    }

    x <- x[is.finite(x)]
    if (length(x) == 0L) {
      return(NA_real_)
    }
    z <- sort((x - xmin) / (xmax - xmin))
    z <- pmin(pmax(z, 0), 1)
    gaps <- diff(c(0, z, 1))
    mean_gap <- mean(gaps)
    if (mean_gap == 0) {
      return(0)
    }
    cv <- stats::sd(gaps) / mean_gap
    return(1 / (1 + cv))
  }

p_abCeil4Data <-
  function (xyP, ilk = "RG") {

    if (ilk == "RG") {
      ab <- p_performRG(xyP)
    } else {
      ab <- p_performLP(xyP)
    }

    if (ab["b"] <= 0) {
      # no horizontal or downward slope
      ab["b"] <- 1e-12
    }

    return(ab)
  }

p_performLP <-
  function (xyP, weights = NULL, hallObjective = FALSE) {
    x.low <- min(xyP[, 'X'])
    y.low <- min(xyP[, 'Y'])
    xyP[, 'X'] <- xyP[, 'X'] - x.low
    xyP[, 'Y'] <- xyP[, 'Y'] - y.low
    numRows <- nrow(xyP)
    sway <- rep(1, numRows) # leads to equal weights
    # TODO I don't think this works, ordering should be over rows not columns?
    if (!(is.null(weights))) { # so we are weighing the points
      sway <- xyP[, weights]   # the weights = data in column/mast
    }
    if (hallObjective) {
      obj <- c(1, 1 / 2) # the Hall objective
    } else {
      obj <- c(sum(sway), sum(sway * xyP[, 'X']))   # "ordinary" objective coefficients
    }

    # lhs of constraints (maybe weighted)
    leftside <- cbind(rep(1, numRows), xyP[, 'X'])
    # direction of constraint(s)
    inEq <- rep(">=", numRows)
    # rhs of constraints
    rightside <- xyP[, 'Y']

    # LP with shifted data inputs
    LP_ <- lp("min", obj, leftside, inEq, rightside)
    # coefficients wrt shifted data
    a_ <- LP_$solution[1]
    b_ <- LP_$solution[2]
    # nb: a gets measured at x = 0!
    a <- a_ - b_ * x.low + y.low

    return(c(a = a, b = b_)) # line: y = a + b x, so raw ab
  }

p_performRG <-
  function (xyP) {
    # do regression of y on x
    ab <- c(a = 0, b = 1)
    xyP <- as.data.frame(xyP)
    if (nrow(xyP) > 1) {
      LM <- lm(Y ~ X, data = xyP)
      ab <- c(a = unname(LM$coefficients[1]), b = unname(LM$coefficients[2]))
    }
    return(ab)
  }

p_intersectLineBox <-
  function (ab, scope) {
    # crossings of box with line and es

    dx <- scope[2] - scope[1]
    dy <- scope[4] - scope[3]
    a <- ab[1]
    b <- ab[2]

    if (dx * dy <= 0) {
      return(NULL)
    }
    if (b <= 0) {
      return(NULL)
    }

    miss <- FALSE # does the ab-line miss the box?
    # intersection y = a + b x with x = bb$x.low:
    wi <- c(scope[1], a + b * scope[1]) # west intersection
    # intersection y = a + b x with y = bb$y.low:
    si <- c((scope[3] - a) / b, scope[3]) # south intersection

    if (wi[1] < si[1]) {
      swi <- si
    } else {
      # pick most eastern point
      swi <- wi
    }
    if (swi[1] > scope[2]) {
      # box to north-west of line
      miss <- TRUE
      es <- 1
    }
    if (swi[2] > scope[4]) {
      # box to south-east of line
      miss <- TRUE
      es <- 0
    }

    # intersection y = a + b x with x = bb$x.high:
    ei <- c(scope[2], a + b * scope[2]) # east intersection
    # intersection y = a + b x with y = bb$y.high:
    ni <- c((scope[4] - a) / b, scope[4]) # north intersection
    # pick most western point
    if (scope[2] > (scope[4] - a) / b) {
      nei <- ni
    } else {
      nei <- ei
    }
    if (nei[1] < scope[1]) {
      # box to south-east of line
      miss <- TRUE
      es <- 0
    }
    if (nei[2] < scope[3]) {
      # box (unreasonably) to north-west of line
      miss <- TRUE
      es <- 1
    }

    if (!miss) {
      # regular case: no miss and two intersections swi and nei with box's boundary
      es <- (swi[1] - scope[1]) * dy
      es <- es + (1 / 2) *
        (nei[1] - swi[1]) *
        (2 * scope[4] - swi[2] - nei[2])
    }

    # "box-line" intersections
    blis <- list(wi = wi, si = si, ei = ei, ni = ni)

    return(list(es = es / (dx * dy), swi = swi, nei = nei, blis = blis,
                inline = c(a = a, b = b), inbox = scope))
  }
