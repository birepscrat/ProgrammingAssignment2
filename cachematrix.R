## makeCacheMatrix takes a matrix and returns a series of functions
## that can be used to solve the inverse of the matrix and store
## that solution for later use

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(solve) inv <<- solve
  getinv <- function() inv
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}

cacheSolve <- function(x, ...) {
    ## given an object of makeCacheMatrix(), either calculates the inverse of the matrix
    ## or returns the cached value if called more than once, saving computing resources.
    inv <- x$getinv()
    if(!is.null(inv)) {
      return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setinv(inv)
    inv
}
