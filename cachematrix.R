#### Programming Assignment 2 - Caching the Inverse of a Matrix ###

## The first function, makeCacheMatrix creates a matrix having a function to
## creating a matrix defining and obtaining the inverse matrix.

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


## The second function checks to see if the inverse of the matrix has been solved and cached. 
## If so, it returns the cached matrix.  If not, it then solves the matrix. And caches the inverse of a matrix.

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
