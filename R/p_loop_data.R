p_create_loop_data <-
function (x, y, scope, flip.x, flip.y, id.x, qr.tau) {
  names <- c(colnames(x), colnames(y))
  x <- x[id.x]
  scope <- scope[[id.x]]
  flip.x <- flip.x[id.x]

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
    scope.theo <- c(min(scope.emp[1], scope[c(1, 2)], na.rm=TRUE),
                    max(scope.emp[2], scope[c(1, 2)], na.rm=TRUE),
                    min(scope.emp[3], scope[c(3, 4)], na.rm=TRUE),
                    max(scope.emp[4], scope[c(3, 4)], na.rm=TRUE))
    if (any(sort(scope[1:2]) != sort(scope.theo[1:2])) ||
        any(sort(scope[3:4]) != sort(scope.theo[3:4]))) {
      # Needed to collapse multiple scope warnings
      if (is.null(.GlobalEnv$scope_warnings_nca)) {
        .GlobalEnv$scope_warnings_nca <- TRUE
        warning(
          "Theorectical scope has been adjusted to include all observations", call.=FALSE)
      }
    }
  }
  scope.area  <- (scope.theo[2] - scope.theo[1]) * (scope.theo[4] - scope.theo[3])

  return (list(x=as.matrix(x), y=as.matrix(y), idx=id.x,
               scope.emp=scope.emp, scope.theo=scope.theo,
               scope.area=scope.area, names=names,
               flip.x=flip.x, flip.y=flip.y,
               qr.tau=qr.tau))
}
