## The two functions makeCacheMatrix and cacheSolve
## allow you to get the inverse of a matrix,
## using caching to return the inverse if it's already been computed.

## Pass in an invertable matrix, set the (previous) cached matrix to NULL, 
## and return a list of four functions: set and get (for the matrix) and
## setinverse and getinverse (for the inverse of the matrix)
makeCacheMatrix <- function(x = matrix()) {
  invrs <- NULL
 #set new x in cache; reset old inverse
  set <- function(new_x) {
    x <<- new_x
    invrs <<- NULL
  }
  get <- function() x
  setinverse <- function(new_invrs) invrs <<- new_invrs
  getinverse <- function() invrs
  #return a list of 4 functions
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

## Pass in your result from makeCacheMatrix into this function
## to solve for the inverse, first checking if the cached invrs already 
## has a stored value (and if so, return that before getting to solve())
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  invrs <- x$getinverse()
  # if invrs already has a value...
  if(!is.null(invrs)) {
    message("getting cached data")
    return(invrs)
  }
  # else, compute the inverse
  current_matrix <- x$get()
  invrs <- solve(current_matrix, ...)
  #set the new inverse
  x$setinverse(invrs)
  invrs
}
