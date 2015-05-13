## This set of functions allows to compute and calculate the inverse of a matrix.

## The makeCacheMatrix function creates a matrix "object" (a list consisting of functions to get and
## set a matrix as well as get and set the (cached) inverse of the matrix.
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) m <<- inverse
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Compute the inverse of a matrix "object" returned by makeCacheMatrix. If
## the inverse has already been calculated, it returns the cached result of this
## inversion.
cacheSolve <- function(x, ...) {
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}
