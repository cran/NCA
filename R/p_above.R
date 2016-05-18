p_above <- 
function (loop.data, slope, intercept) {
  x <- loop.data$x
  y <- loop.data$y
  flip.x <- loop.data$flip.x
  flip.y <- loop.data$flip.y

  # Upper left and lower right
  if ( (flip.x == flip.y) && (is.na(slope) || slope < 0) ) {
    return( NaN )
  }
  # Lower left and upper right
  if ( (flip.x != flip.y) && (is.na(slope) || slope > 0) ) {
    return( NaN )
  }

  # Vertical difference between observations and ceiling line
  y.c <- slope * x + intercept
  if ( !flip.y ) {
    y.diff <- y - y.c
  } else {
    y.diff <- y.c - y
  }

  return ( sum(y.diff > 1e-07, na.rm=TRUE) )
}