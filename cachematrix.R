#### Programming Assignment 2 - Caching the Inverse of a Matrix ###

## The first function, makeCacheMatrix creates a matrix having a function to
## creating a matrix defining and obtaining the inverse matrix.

makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL
  set <- function(y) {
    x <<- y
    inverse <<- NULL
  }
  get <- function() x
  setinverse <- function(inv) inverse <<- inv
  getinverse <- function() inverse
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)

}



## The second function checks to see if the inverse of the matrix has been solved and cached. 
## If so, it returns the cached matrix.  If not, it then solves the matrix. And caches the inverse of a matrix.

cacheSolve <- function(x, ...) {
  inverse <- x$getinverse()
  if(!is.null(inverse)) {
    message("getting cached data")
    return(inverse)
  }
  data <- x$get()
  inverse <- solve(data, ...)
  x$setinverse(inverse)
  inverse
  
}
