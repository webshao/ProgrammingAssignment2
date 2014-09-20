## This file defines a container for a matrix with getter and
## setter methods for a matrix and its inverse, and a function
## for returning the inverse of a matrix.

## Defines a container for setting and getting a matrix and
## its inverse

makeCacheMatrix <- function(x = matrix()) {
  ivrs <- NULL
  set <- function(y) {
    x <<- y
    ivrs <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) ivrs <<- inverse
  getInverse <- function() ivrs
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}

## Defines a cache version of the matrix inversion function.
## This computes the matrix inverse of the input if it has not
## been computed before. If it has been computed, this returns
## the cached result of the computation.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  ivrs <- x$getInverse()
  if (!is.null(ivrs)) {
    message("retrieving cached data")
    return(ivrs)
  }
  data <- x$get()
  ivrs <- solve(data, ...)
  x$setInverse(ivrs)
  ivrs
}

