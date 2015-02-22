## The functions cache the inverse of a Matrix rather than recomputing it


# This fuction creates a matrix with a list of functions associated with it

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(inverse) inv <<- inverse
  getinv <- function() inv
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}

# This function checks if the inverse has already been calculated (subject to 
# the matrix not getting changed). if so, it retrieves the result from the cache
# and skips the calculation. Otherwise it calculates & returns the inverse of the 
# matrix.

cacheSolve <- function(x, ...) {
  inv <- x$getinv()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinv(inv)
  inv
}