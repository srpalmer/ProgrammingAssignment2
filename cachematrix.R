## makeCacheMatrix and cacheSolve leverage the scoping rules in R to store a 
## value in the cache, potentially saving time on long and/or repeated operations.

## 1. Creates a matrix and calculates the inverse, which is stored in cache.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) m <<- solve
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## 2. Checks to see if an inverse on x has been calculated and retrieves it. 
## Otherwise solves for the inverse.

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
