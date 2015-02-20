## The next functions are used for caching
## the inverse of a matrix

## This function creates a special object matrix object
## than can cache the inverse of a matrix

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


## This function calculates the inverse of a matrix
## and detects if the inverse matrix have been cached.

cacheSolve <- function(x, ...) {
## Returns the cached value of the inverse matrix to m  
  m <- x$getsolve()
## Check if inverse matrix was cached before
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
data <- x$get()
  m <- solve(data, ...)
  x$setsolve(m)
## Return a matrix that is the inverse of 'x'
  m
}
