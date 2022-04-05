p_columns <-
function (loop.data, is.confidence) {
  flip.x <- loop.data$flip.x
  flip.y <- loop.data$flip.y
  x.sorted <- loop.data$x[order(loop.data$x, decreasing=flip.x)]
  y.sorted <- loop.data$y[order(loop.data$x, decreasing=flip.x)]

  columns <- p_initial_columns (x.sorted, y.sorted, loop.data, flip.x, flip.y)
  columns <- p_merge_columns(columns, flip.x, flip.y)

  if (is.confidence) {
    columns <- p_bootstrap(y.sorted, columns, loop.data)
    columns <- p_con_ce(columns, loop.data)
  }

  return ( columns )
}

p_initial_columns <-
function (x, y, loop.data, flip.x, flip.y) {
  # Initial nbr and widths of columns
  ux <- unique(x)
  k  <- length(ux)
  colwidth <- ux[-1] - ux[-k]

  # Get the boundaries from the unique x values and the column widths
  boundries <- ux[-k] + colwidth/2
  if (!flip.x) {
    first <- min(loop.data$scope.theo[1], ux[1] - colwidth[1]/2)
    second <- max(loop.data$scope.theo[2], ux[k] + colwidth[k-1]/2)
  } else {
    first <- max(loop.data$scope.theo[2], ux[1] - colwidth[1]/2)
    second <- min(loop.data$scope.theo[1], ux[k] + colwidth[k-1]/2)
  }
  boundries <- c(first, boundries, second)

  # Row 1 : nof datapoints in the column
  # Row 2 + 3 : x boundaries of the columns
  # Row 4 : x value of the max(y) datapoint
  # Row 5 : first max(y) of the datapoints, changes into bootstrap value

  columns <- matrix(0, nrow=5, ncol=k)
  for (i in 1:k) {
    # Get the boudries for the columns
    cumm1 <- sum(columns[1,]) + 1
    if (!flip.x) {
      count <- length(x[x >= boundries[i] & x < boundries[i+1] ])
    } else {
      count <- length(x[x <= boundries[i] & x > boundries[i+1] ])
    }
    columns[1:3,i] <- c(count, boundries[i], boundries[i+1])

    cumm2 <- sum(columns[1,])
    # No points in this column
    if (cumm1 > cumm2) {
      y.max <- loop.data$scope.theo[3 + flip.y]
      x.max <- loop.data$scope.theo[1 + flip.x]
    } else {
      y.max <- ifelse(flip.y, min(y[cumm1:cumm2]), max(y[cumm1:cumm2]))
      id <- intersect(cumm1:cumm2, which(y==y.max))
      id <- ifelse(!flip.x, head(id, n=1), tail(id, n=1))
      x.max <- x[id]
    }

    columns[4:5, i] <- c(x.max, y.max)
  }

  return ( columns )
}

p_merge_columns <-
function (columns, flip.x, flip.y) {
  # Min nof datapoint per column
  min.count <- round( sqrt(sum(columns[1,]) / 2) )

  while (TRUE) {
    # No more columns to merged
    if (ncol(columns) <= 1) {
      break
    }

    # If the smallest column has enough points we're done
    if (min(columns[1, ]) >= min.count) {
      break
    }

    # Search for (leftmost) largest below min.count
    max.below.min <- max(columns[1,][which(columns[1,] < min.count)])
    min.col <- min(which(columns[1,]==max.below.min))

    # For outer columns there's no choice
    if (min.col == 1 || min.col == ncol(columns)) {
      min.neighbor <- min.col + ifelse(min.col == 1, 1, -1)
    } else {
      # Find (left most) smallest above min.count OR largest below min.count
      neighbor.cols <- c(min.col - 1, min.col + 1)
      values <- columns[1, neighbor.cols]

      if ( min(values) >= min.count ) {
        # Find smallest (left most) above min.count for both above
        min.neighbor <- min.col + ifelse(values[1] > values[2], 1, -1)
      } else {
        # Find largest (left most) for both below or mixed
        min.neighbor <- min.col + ifelse(values[2] > values[1], 1, -1)
      }
    }

    columns <- p_merge_2_columns(columns, min.col, min.neighbor, flip.x, flip.y)
  }

  return (columns)
}

p_merge_2_columns <-
function (columns, col1, col2, flip.x, flip.y) {
  # Add the counts
  columns[1, col1] <- columns[1, col1] + columns[1, col2]

  # Then move the left or right 'border'
  if (col1 > col2) {
    columns[2, col1] <- columns[2, col2]
  } else {
    columns[3, col1] <- columns[3, col2]
  }

  # Use highest (or lowest on flip.y) points
  if (!flip.y && columns[5, col1] < columns[5, col2]) {
    columns[4, col1] <- columns[4, col2]
    columns[5, col1] <- columns[5, col2]
  } else if (flip.y && columns[5, col1] > columns[5, col2]) {
    columns[4, col1] <- columns[4, col2]
    columns[5, col1] <- columns[5, col2]
  } else if (columns[5, col1] == columns[5, col2]) {
    columns[5, col1] <- p_if_min_else_max(!flip.x, columns[5, col1], columns[5, col2])
  }

  # Delete the column
  columns <- columns[,-col2]

  # Make sure we have a matrix
  if (is.null(dim(columns))) {
    columns <- matrix(columns , ncol = 1)
  }

  return ( columns )
}

p_bootstrap <-
function (y, columns, loop.data) {
  conf <- loop.data$conf
  conf.rep <- loop.data$conf.rep
  flip.y <- loop.data$flip.y

  # Normalize the y values
  y.min   <- ifelse(!flip.y, min(y), max(y))
  y.range <- ifelse(!flip.y, max(y) - min(y), min(y) - max(y))
  y <- (y - y.min) / y.range

  start <- 1
  for (col in 1:ncol(columns)) {
    end <- start + columns[1, col] - 1
    ci <- p_bootstrap_column(y[start:end], columns[1, col], conf, conf.rep)
    columns[5, col] <- (ci * y.range) + y.min
    start <- end + 1
  }

  return ( columns )
}

# Smooth Bootstrap (adapted from 'Estimation and Inference in Nonparametric
# Frontier Models: Recent Developments and Perspectives'
# By Leopold Simar and Paul W. Wilson, page 237)
p_bootstrap_column <-
function (z, n, conf, nrep) {
  n2        <- n * 2
  zeta.hat  <- max(z)
  zeta.star <- vector(length=nrep)

  for (b in 1:nrep) {
    # reflection method (Silverman, 1986) to restore consistency
    zr <- c(z, 2 * zeta.hat - z)
    # add a mirror-image of the data to the data to create set Z^R_n
    # draw n random integers on [1,..,n*2]
    ind <- floor( runif(n) * n2 + 1 )
    # Naive bootstrap: random sample of size n from Z^R_n
    # (independently, uniformly, and with replacement)
    zs <- zr[ind]
    # determine  bandwidth h to estimate
    # the density of the 2n elements in Z^R_n via plug-in method
    hr <- p_dpikSafe(zr)
    # (Sheather and Jones, 1991) with the reflected data
    # rescale bandwidth by multiplying by 2^(1/5)
    # since the optimal bandwidth is of order O(n^(???1/5))
    h <- (2 ^ 0.2) * hr
    # instead of current O((2n)^(???1/5));
    # we only have n real obeservations instead of 2n.
    t1 <- zs + h * rnorm(n)
    zss <- ifelse(t1 <= zeta.hat, t1, 2 * zeta.hat - t1)
    # sample mean of naive bootstrap sample
    t2 <- mean(zs)
    # It can be shown that the Zsss behave as draws from \hat{f}_h(t) \
    # with E(zsss_i | Z_n) = the real sample mean.
    zsss <- t2 + (zss - t2) / sqrt( 1+(h^2) / var(zss) )
    # maxima of Zsss sample
    zeta.star[b] <- max(zsss)
  }

  # sample variance is zeta.hat/n
  g.star <- (n/zeta.hat) * (zeta.hat-zeta.star)
  qq <- quantile(g.star, probs=conf)
  ci <- zeta.hat / (1 - qq/n)

  return ( ci )
}

# Function to handle cases where dpik() is unable to estimate
# a bandwidth, usually because a data vector has an interquartile range of 0.
p_dpikSafe <- function(x)
{
  result <- try(dpik(x), silent = TRUE)

  if (typeof(result) == "double") {
    return(result)
  }

  msg <- geterrmessage()
  if (grepl("scale estimate is zero for input data", msg)) {
    return(dpik(x, scalest = "stdev"))
  } else {
    stop(msg)
  }
}

p_con_ce <-
function (columns, loop.data) {
  flip.x <- loop.data$flip.x
  flip.y <- loop.data$flip.y
  scope.theo <- loop.data$scope.theo
  peers <- loop.data$ce_fdh_peers

  # Limit the confidence by the theoretical scope
  columns[5,] <-ifelse(columns[5,] > scope.theo[4], scope.theo[4], columns[5,])
  columns[5,] <-ifelse(columns[5,] < scope.theo[3], scope.theo[3], columns[5,])

  # Limit the confidence by the CE-FDH
  for (col in 1:ncol(columns)) {
    if (flip.x) {
      peer.max = max(which(peers[,1] >= columns[3, col]))
    } else {
      peer.max = max(which(peers[,1] <= columns[3, col]))
    }

    if (flip.y) {
      columns[5, col] <- min(columns[5, col], peers[peer.max, 2])
    } else {
      columns[5, col] <- max(columns[5, col], peers[peer.max, 2])
    }
  }

  return ( columns )
}

p_con_ce_org <-
function (columns, flip.x, flip.y) {
  i <- 1
  while (i < ncol(columns)) {
    # Merge if left hand column is bigger than right hand column# Or
    if (columns[5, i + flip.y] >= columns[5, i + !flip.y]) {
      columns <- p_merge_2_columns(columns, i, i + 1, flip.x, flip.y)
    } else {
      i <- i + 1
    }

    if (is.vector(columns)) {
      columns <- matrix(columns, ncol=1)
      break
    }
  }

  return ( columns )
}

p_conf_line <-
function (columns) {
  x.points <- c()
  y.points <- c()
  for (col in 1:ncol(columns)) {
    x.points <- c(x.points, columns[2, col], columns[3, col])
    y.points <- c(y.points, columns[5, col], columns[5, col])
  }
  return (list(x.points, y.points))
}
