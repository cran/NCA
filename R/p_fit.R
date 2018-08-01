get_fit <- function(ceiling, fdh_ceiling) {
  return(100 - 100 * abs(ceiling - fdh_ceiling) / fdh_ceiling)
}
