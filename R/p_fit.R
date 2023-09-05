get_fit <- function(ceiling, fdh_ceiling) {
  if (is.nan(ceiling) || is.nan(fdh_ceiling)) {
    return ( NA )
  }
  if (ceiling > fdh_ceiling) {
    return ( NA )
  }
  return(100 - 100 * abs(ceiling - fdh_ceiling) / fdh_ceiling)
}
