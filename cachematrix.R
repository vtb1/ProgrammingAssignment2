## The next functions are used for caching
## the inverse of a matrix

## This function creates a special object matrix object
## than can cache the inverse of a matrix

makeCacheMatrix <- function(x = matrix()) {
## Initialize m inverse matrix  to Null
  m <- NULL
## Set the value of the matrix
    set <- function(y) {
        x <<- y
        m <<- NULL
        }
## get the value of the matrix
    get <- function() x
## SetSolve calculates the inverse of the matrix
    setsolve <- function(solve) m <<- solve
## getsolve gets the value of m inverse of a matrix
   getsolve <- function() m
## List of functions
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
## If is not cached then calculate the inverse matrix 
  data <- x$get()
  m <- solve(data, ...)
  x$setsolve(m)
## Return a matrix that is the inverse of 'x'
  m
}
