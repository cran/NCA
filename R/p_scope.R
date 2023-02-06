p_scope <-
function (x, scope) {
  if (is.null(scope)) {
    return ( replicate(length(x), NULL) )
  } else if (typeof(scope) == "double") {
    return ( p_validate_scope_vector(x, scope) )
  } else if (typeof(scope) == "list") {
    return ( p_validate_scope_list(x, scope) )
  }
}

p_validate_scope_list <-
function (x, scope) {
  if (length(scope) != length(x)) {
    message()
    stop("The length of scope needs to be equal to the length of x \n",
         "       or a single vector with 4 values!\n\n", call.=F)
  }

  for (i in scope) {
    if (length(i) != 4) {
      message()
      stop("The length of each scope segment needs to be 4!\n\n", call.=F)
    }
  }

  for (i in scope) {
    same <- i[c(3, 4)] == scope[[1]][c(3, 4)]
    same <- same | (is.na(i[c(3, 4)]) & is.na(scope[[1]][c(3, 4)]))
    same[is.na(same)] <- FALSE
    if (!all(same)) {
      warning("Scope is using different Ymin and Ymax for the same Y", call.=F)
      break
    }
  }

  return (scope)
}

p_validate_scope_vector <-
function (x, scope) {
  # Duplicate single vector for all Xs
  if (length(scope) == 4) {
    scope <- rep(scope, length(x))
  }

  # Vector with all values, transform to list of vectors
  if (length(scope) == 4 * length(x)) {
    return (split(matrix(scope, nrow=4), rep(1:length(x), each=4)))
  }

  message()
  stop("The length of scope needs to be equal to the length of x \n",
       "       or a single vector with 4 values!\n\n", call.=F)
}