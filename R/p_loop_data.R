p_create_loop_data <-
function (x, y, scope, flip.x, flip.y, id.x, weighting) {
  names <- c(colnames(x), colnames(y))
  x <- x[id.x]
  scope <- scope[[id.x]]
  flip.x <- flip.x[id.x]
  weighting <- isTRUE(weighting)

  # Remove not complete cases
  tmp <- cbind(x, y)
  tmp <- tmp[complete.cases(tmp),]
  x <- tmp[-length(tmp)]
  y <- tmp[length(tmp)]

  # Define the scope params
  scope.emp <- c(min(x), max(x), min(y), max(y))
  if (is.null(scope)) {
    scope.theo <-  scope.emp
  } else {
    scope.theo <- c(min(scope.emp[1], scope[1]), max(scope.emp[2], scope[2]),
                    min(scope.emp[3], scope[3]), max(scope.emp[4], scope[4]))
  }
  scope.area  <- (scope.theo[2] - scope.theo[1]) * (scope.theo[4] - scope.theo[3])

  return (list(x=as.matrix(x), y=as.matrix(y), idx=id.x,
               scope.emp=scope.emp, scope.theo=scope.theo,
               scope.area=scope.area, names=names,
               flip.x=flip.x, flip.y=flip.y, weighting=weighting))
}