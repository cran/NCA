CREATE_MAZE <- paste(
  "CREATE TABLE maze",
  "AS SELECT",

  "'maze_' || ROW_NUMBER() OVER (ORDER by '') AS mazeID,",

  "rp.col AS e_col,",
  "rp.se_x AS e_x,",
  "rp.cardDirEW AS 'cardDirEW.x',",

  "rp.row AS s_row,",
  "rp.se_y AS s_y,",
  "rp.cardDirNS AS 'cardDirNS.x',",
  "rp.se_pos,",

  "r1.'col.y' AS w_col,",
  "r1.nw_x AS w_x,",
  "r1.cardDirEW AS 'cardDirEW.y',",

  "r1.'row.y' AS 'n_row',",
  "r1.nw_y AS n_y,",
  "r1.cardDirNS AS 'cardDirNS.y',",
  "r1.nw_pos,",

  # TODO Not needed?
  "(CASE WHEN 'enlarged' IN (rp.strainx, rp.strainy, r1.strainx, r1.strainy)
    THEN TRUE ELSE FALSE END) AS onEdge,",
  "(CASE WHEN (nw_pos = 'nw' AND se_pos = 'se') THEN 'bisect'
         WHEN (nw_pos = 'on' OR nw_pos = 'se') THEN 'se'
         ELSE 'nw' END) AS stance,",
  "(CASE WHEN (nw_pos = 'nw' AND se_pos = 'se')
         THEN (rp.se_x - r1.nw_x) * (r1.nw_y - rp.se_y) / 2
         ELSE (rp.se_x - r1.nw_x) * (r1.nw_y - rp.se_y)
         END) AS area,",

  "(rp.se_x - :br1) * (:br4 - rp.se_y) AS se_NW_den,",
  "(r1.nw_x - :br1) * (:br4 - r1.nw_y) AS nw_NW_den,",
  "(r1.nw_x - :br1) * (:br4 - rp.se_y) AS sw_NW_den,",
  "(rp.se_x - :br1) * (:br4 - r1.nw_y) AS ne_NW_den,",

  "(:br2 - rp.se_x) * (rp.se_y - :br3) AS se_SE_den,",
  "(:br2 - r1.nw_x) * (r1.nw_y - :br3) AS nw_SE_den,",
  "(:br2 - r1.nw_x) * (rp.se_y - :br3) AS sw_SE_den,",
  "(:br2 - rp.se_x) * (r1.nw_y - :br3) AS ne_SE_den,",

  "NULL AS se_NW_num,",
  "NULL AS nw_NW_num,",
  "NULL AS sw_NW_num,",
  "NULL AS ne_NW_num,",

  "NULL AS se_SE_num,",
  "NULL AS nw_SE_num,",
  "NULL AS sw_SE_num,",
  "NULL AS ne_SE_num,",

  "(CASE WHEN (r1.nw_x = :br1 AND r1.nw_y = :br4) THEN 'north-west'
         WHEN r1.nw_x = :br1 THEN 'west'
         WHEN r1.nw_y = :br4 THEN 'north'
         ELSE 'other' END) AS nw_loc,",
  "(CASE WHEN (rp.se_x = :br2 AND rp.se_y = :br3) THEN 'south-east'
         WHEN rp.se_x = :br2 THEN 'east'
         WHEN rp.se_y = :br3 THEN 'south'
         ELSE 'other' END) AS se_loc,",

  "colrangebyrow.'x.high' AS vx,",
  "colrangebyrow.'col.high' AS vc,",
  "rowrangebycol.'y.high' AS hy,",
  "rowrangebycol.'row.high' AS hr,",

  "NULL AS vslope,",
  "NULL AS hslope,",

  "NULL AS sw_insol,",
  "NULL AS ne_insol,",
  "NULL AS nw_insol,",
  "NULL AS se_insol,",

  "NULL AS sw_inver,",
  "NULL AS ne_inver,",
  "NULL AS nw_inver,",
  "NULL AS se_inver,",

  # TODO Not needed?
  "NULL AS se_inacc,",
  "NULL AS sw_inacc,",
  "NULL AS nw_inacc,",
  "NULL AS ne_inacc,",

  "NULL AS nesw_sqr",

  "\n",
  "FROM rp",
  "JOIN r1 ON rp.col = r1.col AND rp.row = r1.row",
  "JOIN colrangebyrow ON rp.row = colrangebyrow.row",
  "JOIN rowrangebycol ON r1.'col.y' = rowrangebycol.col",

  "WHERE area > 0",
  "ORDER BY e_col, e_x, s_y;")

# TODO Merge updates for efficiency?
UPDATE_MAZE_1 <- "UPDATE maze SET
    se_NW_num = (SELECT COALESCE(SUM(m2.area), 0)
                 FROM maze m2
                 WHERE m2.e_col <= maze.e_col AND m2.s_row >= maze.s_row
                       AND m2.stance IN ('bisect', 'se')),
    nw_NW_num = (SELECT COALESCE(SUM(m2.area), 0)
                 FROM maze m2
                 WHERE m2.e_col <= maze.w_col AND m2.s_row >= maze.n_row
                       AND m2.stance IN ('bisect', 'se')),
    sw_NW_num = (SELECT COALESCE(SUM(m2.area), 0)
                 FROM maze m2
                 WHERE m2.e_col <= maze.w_col AND m2.s_row >= maze.s_row
                       AND m2.stance IN ('bisect', 'se')),
    ne_NW_num = (SELECT COALESCE(SUM(m2.area), 0)
                 FROM maze m2
                 WHERE m2.e_col <= maze.e_col AND m2.s_row >= maze.n_row
                       AND m2.stance IN ('bisect', 'se'));"

UPDATE_MAZE_2 <- "UPDATE maze SET
    se_SE_num = (SELECT COALESCE(SUM(m2.area), 0)
                 FROM maze m2
                 WHERE m2.w_col >= maze.e_col AND m2.n_row <= maze.s_row
                       AND m2.stance IN ('bisect', 'nw')),
    nw_SE_num = (SELECT COALESCE(SUM(m2.area), 0)
                 FROM maze m2
                 WHERE m2.w_col >= maze.w_col AND m2.n_row <= maze.n_row
                       AND m2.stance IN ('bisect', 'nw')),
    sw_SE_num = (SELECT COALESCE(SUM(m2.area), 0)
                 FROM maze m2
                 WHERE m2.w_col >= maze.w_col AND m2.n_row <= maze.s_row
                       AND m2.stance IN ('bisect', 'nw')),
    ne_SE_num = (SELECT COALESCE(SUM(m2.area), 0)
                 FROM maze m2
                 WHERE m2.w_col >= maze.e_col AND m2.n_row <= maze.n_row
                       AND m2.stance IN ('bisect', 'nw'));"

UPDATE_MAZE_3 <- "UPDATE maze SET
    vslope = (SELECT (CASE WHEN lm.stance = 'bisect'
                           THEN (lm.n_y - lm.s_y) / (lm.e_x - lm.w_x)
                           ELSE 1e999 END)
              FROM maze lm
              WHERE maze.vc = lm.w_col AND maze.s_row = lm.s_row),
    hslope = (SELECT (CASE WHEN lm.stance = 'bisect'
                           THEN (lm.n_y - lm.s_y) / (lm.e_x - lm.w_x)
                           ELSE 0 END)
              FROM maze lm
              WHERE maze.hr = lm.s_row AND maze.w_col = lm.w_col);"

UPDATE_MAZE_4 <- "UPDATE maze SET
    sw_insol = (CASE WHEN stance IN ('bisect', 'nw') THEN 0
                     WHEN nw_loc = 'west' THEN (hy - s_y) / (:br4 - s_y)
                     ELSE sw_NW_num / sw_NW_den END),
    ne_insol = (CASE WHEN stance IN ('bisect', 'nw') THEN 0
                     WHEN nw_loc = 'north' THEN (e_x - w_x) / (e_x - :br1)
                     ELSE ne_NW_num / ne_NW_den END),
    nw_insol = (CASE WHEN (stance IN ('bisect', 'nw') OR nw_NW_den = 0) THEN 0
                     ELSE nw_NW_num / nw_NW_den END),
    se_insol = se_NW_num / se_NW_den,

    sw_inver = (CASE WHEN stance IN ('bisect', 'se') THEN 0
                     WHEN se_loc = 'south' THEN (e_x - w_x) / (:br2 - w_x)
                     ELSE sw_SE_num / sw_SE_den END),
    ne_inver = (CASE WHEN stance IN ('bisect', 'se') THEN 0
                     WHEN se_loc = 'east' THEN (n_y - s_y) / (n_y - :br3)
                     ELSE ne_SE_num / ne_SE_den END),
    nw_inver = nw_SE_num / nw_SE_den,
    se_inver = (CASE WHEN (stance IN ('bisect', 'se') OR se_SE_den = 0) THEN 0
                     ELSE se_SE_num / se_SE_den END),

    nesw_sqr = (CASE WHEN (w_x = :br1 AND s_y = :br3 AND stance != 'bisect') THEN 'sw_sqr'
                     WHEN (e_x = :br2 AND n_y = :br4 AND stance != 'bisect') THEN 'ne_sqr'
                     ELSE 'other' END);"

UPDATE_MAZE_5 <- "UPDATE maze SET
    se_inacc  = MAX(se_insol, se_inver),
    sw_inacc  = MAX(sw_insol, sw_inver),
    nw_inacc  = MAX(nw_insol, nw_inver),
    ne_inacc  = MAX(ne_insol, ne_inver);"

UPDATE_RP <- "UPDATE rp SET
      insol = (CASE WHEN (cardDirEW != 'east' AND cardDirNS != 'north')
                    THEN (SELECT sw_insol FROM maze WHERE maze.w_x = se_x AND maze.s_y = se_y)
                    WHEN (cardDirEW != 'east' AND cardDirNS = 'north')
                    THEN (SELECT nw_insol FROM maze WHERE maze.w_x = se_x AND maze.n_y = se_y)
                    WHEN (cardDirEW = 'east' AND cardDirNS != 'south')
                    THEN (SELECT ne_insol FROM maze WHERE maze.e_x = se_x AND maze.n_y = se_y)
                    WHEN (cardDirEW = 'east' AND cardDirNS = 'south')
                    THEN (SELECT se_insol FROM maze WHERE maze.e_x = se_x AND maze.s_y = se_y)
                    END),
      inver = (CASE WHEN (cardDirEW != 'east' AND cardDirNS != 'north')
                    THEN (SELECT sw_inver FROM maze WHERE maze.w_x = se_x AND maze.s_y = se_y)
                    WHEN (cardDirEW != 'east' AND cardDirNS = 'north')
                    THEN (SELECT nw_inver FROM maze WHERE maze.w_x = se_x AND maze.n_y = se_y)
                    WHEN (cardDirEW = 'east' AND cardDirNS != 'south')
                    THEN (SELECT ne_inver FROM maze WHERE maze.e_x = se_x AND maze.n_y = se_y)
                    WHEN (cardDirEW = 'east' AND cardDirNS = 'south')
                    THEN (SELECT se_inver FROM maze WHERE maze.e_x = se_x AND maze.s_y = se_y)
                    END);"

##############################################################################
# SV 4 POINTS
##############################################################################
CREATE_STANCE <- "
    CREATE TABLE stance AS
      SELECT xyP.x, xyP.y, GROUP_CONCAT(mz.stance) as stance
			FROM mz, xyP
			WHERE xyP.x >= mz.w_x AND xyP.x <= mz.e_x AND xyP.y >= mz.s_y AND xyP.y <= mz.n_y
			GROUP BY xyP.x, xyP.y;"

CREATE_E1 <- "
    CREATE TABLE e1 AS
      SELECT *
      FROM (
        SELECT xyP.x, xyP.y, mz.*, ROW_NUMBER() OVER (PARTITION BY xyP.x, xyP.y ORDER BY mz.e_x, mz.s_y) rn
        FROM mz, xyP
        WHERE xyP.x >= mz.w_x AND xyP.x <= mz.e_x AND xyP.y >= mz.s_y AND xyP.y <= mz.n_y
      )
      WHERE rn == 1;"

UPDATE_XYP_1 <- "
    UPDATE xyP SET
    se = (SELECT stance.stance FROM stance WHERE stance.x = xyP.x AND stance.y = xyP.y) LIKE '%se%',
    nw = (SELECT stance.stance FROM stance WHERE stance.x = xyP.x AND stance.y = xyP.y) LIKE '%nw%';"

UPDATE_XYP_2 <- "
    UPDATE xyP SET
    feas = (CASE WHEN se = 1 THEN TRUE
                 WHEN e1.stance = 'bisect'
                 THEN (xyP.y - e1.s_y) * (e1.e_x - e1.w_x) <= (xyP.x - e1.w_x) * (e1.n_y - e1.s_y)
                 ELSE FALSE END),
    ezon = (CASE WHEN nw = 1 THEN TRUE
                 WHEN e1.stance = 'bisect'
                 THEN (xyP.y - e1.s_y) * (e1.e_x - e1.w_x) >= (xyP.x - e1.w_x) * (e1.n_y - e1.s_y)
                 ELSE FALSE END),
    ver_den = -(xyP.x - e1.w_x) * (xyP.y - e1.s_y) + (xyP.x - e1.w_x) * (:br3 - e1.s_y)
              - (xyP.y - e1.s_y) * (e1.w_x - :br2) + e1.sw_SE_den,
    ver_num = power((xyP.x - e1.w_x), 2) * e1.hslope / 2 - (xyP.x - e1.w_x) * (xyP.y - e1.s_y) +
              power((xyP.y - e1.s_y), 2) / (2 * e1.vslope) + (xyP.x - e1.w_x) * (e1.hy - e1.s_y)
              - (xyP.y - e1.s_y) * (e1.w_x - e1.vx) + e1.sw_SE_num,
    sol_den = -(xyP.x - e1.w_x) * (xyP.y - e1.s_y) + (xyP.x - e1.w_x) * (:br4 - e1.s_y)
              - (xyP.y - e1.s_y) * (e1.w_x - :br1) + e1.sw_NW_den,
    sol_num = power((xyP.x - e1.w_x), 2) * e1.hslope / 2 - (xyP.x - e1.w_x) * (xyP.y - e1.s_y) +
              power((xyP.y - e1.s_y), 2) / (2 * e1.vslope) + (xyP.x - e1.w_x) * (e1.hy - e1.s_y)
              - (xyP.y - e1.s_y) * (e1.w_x - e1.vx) + e1.sw_NW_num
    FROM e1
    WHERE e1.x = xyP.x AND e1.y = xyP.y;"

UPDATE_XYP_3 <- "
    UPDATE xyP SET
    ver_devi = (CASE WHEN xyP.x = :br2 THEN (xyP.y - e1.hy) / (xyP.y - :br3)
                     WHEN xyP.y = :br3 THEN (e1.vx - xyP.x) / (:br2 - xyP.x)
                     WHEN (xyP.x != :br2) AND (xyP.y != :br3)
                     THEN (CASE WHEN (e1.nesw_sqr = 'ne_sqr') AND (ver_den = 0)
                                THEN (xyP.y - e1.w_x) / (xyP.y - :br3)
                                WHEN (e1.nesw_sqr = 'sw_sqr') AND (ver_den = 0)
                                THEN (e1.e_x - xyP.x) / (:br2 - xyP.x)
                                ELSE ver_num / ver_den END)
                     ELSE 0 END),

    sol_devi = (CASE WHEN xyP.x = :br1 THEN(xyP.y - e1.hy) / (xyP.y - :br4)
                     WHEN xyP.y = :br4 THEN(xyP.x - e1.vx) / (:br1 - xyP.x)
                     WHEN (xyP.x != :br1) AND (xyP.y != :br4)
                     THEN (CASE WHEN (e1.nesw_sqr = 'ne_sqr') AND (sol_den = 0)
                                THEN (xyP.x - e1.w_x) / (xyP.x - :br1)
                                WHEN (e1.nesw_sqr = 'sw_sqr') AND (sol_den = 0)
                                THEN (e1.n_y - xyP.y) / (:br4 - xyP.y)
                                ELSE sol_num / sol_den END)
                     ELSE 0 END)

    FROM e1
    WHERE e1.x = xyP.x AND e1.y = xyP.y;"

UPDATE_XYP_4 <- "
    UPDATE xyP SET
    inver = COALESCE(
      (SELECT rp.inver FROM rp WHERE xyP.x = rp.rx AND xyP.y = rp.ry),
      (CASE WHEN feas THEN 0 ELSE ver_devi END)
    ),
    insol = COALESCE(
      (SELECT rp.insol FROM rp WHERE xyP.x = rp.rx AND xyP.y = rp.ry),
      (CASE WHEN ezon THEN 0 ELSE sol_devi END)
    );"
