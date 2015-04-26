## Assignment 2 - Caching the Inverse of a Matrix

## makeCacheMatrix creates a special "matrix" object that can cache
## its inverse

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setsolve <- function(solve) m <<- solve
  getsolve <- function() m
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)

}


## cacheSolve computes the inverse of the special "matrix" returned by
## makeCacheMatrix. If the inverse has already been calculated and the
## matrix has not changed, then cachSolve should retrieve the inverse
## from the cache
m <- matrix(c(-1, -2, 1, 1), 2,2)
cacheSolve <- function(x, ...) {
  m <- x$getsolve()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setsolve(m)
  m
}
