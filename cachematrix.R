
#Below functions calculate the inverse of matrix. The purpose of this is to
#show the caching mechanism using lexical scoping

#
# Returns special vector containing the set of functions to calculate
# inverse
# @param x - matrix whose inverse will be claculated
# @return - list of function to calculate inverse.
#
makeCacheMatrix <- function(x = matrix()) {

  m <- NULL
  pm <- NULL
  set <- function(y) {
    pm <<- x
    x <<- y
    m <<- NULL
  }
  get <- function() x
  
  setinv <- function(inv) m <<- inv
  
  getinv <- function() m
  
  getprev <- function () pm
  
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv, getprev = getprev)
}

#
# Returns the inverse of matrix provided in arguments.
# @param x - The matrix whose inverse to be calculated
# @return - Inverse of provided matrix.
#
cacheSolve <- function(x, ...) {
  m <- x$getinv()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  matrix1 <- x$get()
  matprev <- x$getprev()
  if (!identical(matrix1,matprev))
  {
    message("Recalculating the inverse..")
    m <- solve(matrix1)
    x$setinv(m)
  }
  m
}
