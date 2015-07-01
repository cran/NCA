p_ceiling <- 
function (loop.data, slope, intercept) {
  if (is.na(slope) || slope == 0) {
    return(NaN)
  }
  
  y.a <- slope * loop.data$x.low  + intercept
  y.b <- slope * loop.data$x.high + intercept
  x.c <- (loop.data$y.high - intercept) / slope
  
  if (x.c <= loop.data$x.high) {
    ceiling <- 0.5 * (x.c - loop.data$x.low) * (loop.data$y.high - y.a)
  } else {
    ceiling <- (loop.data$x.high - loop.data$x.low) * (loop.data$y.high - 0.5 * (y.a + y.b))
  }
  
  return (unname(ceiling))
}