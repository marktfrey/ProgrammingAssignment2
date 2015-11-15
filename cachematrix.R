# The functions in this file create a matrix-like object with
#   a cacheable inverse.
#   It is assumed that all matrices are square and invertable.

# Returns a set of methods for interacting with an inverse-cacheable version
#  of the passed-in matrix x.
makeCacheMatrix <- function(x = matrix()) {

  # Store the value of the cacheMatrix in something with an intuitive name
  # so it is easier to keep track of what's in there.
  cacheMatrix <- x

  # Initialize a variable to hold the cached inverse, if it is present
  inverseOfCacheMatrix <- NULL

  # Set the value of cacheMatrix (and reinitialize its inverse)
  set <- function(x = matrix()) {
    cacheMatrix <<- x
    inverseOfCacheMatrix <<- NULL
  }

  # Get the value of cacheMatrix
  get <- function() {
    cacheMatrix
  }

  # Set the inverse of cacheMatrix
  setInverse <- function(inverse) {
    inverseOfCacheMatrix <<- inverse
  }

  # Get the inverse of cacheMatrix
  getInverse <- function() {
    inverseOfCacheMatrix
  }

  # Return the public methods for interacting with the cacheMatrix
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


# Returns a matrix that is the inverse of 'x', where 'x' is a result
#   of calling makeCacheMatrix on a 'regular' matrix.
# If the inverse has already been calculated, returns the cached version.
# If not, set the cache value in the calling scope.
cacheSolve <- function(x, ...) {
  # check the calling scope for a cached inverse, and return it if present.
  inverse <- x$getInverse()
  if (!is.null(inverse)) {
    return (inverse)
  }

  # otherwise, set and return the inverse.
  x$setInverse(solve(x$get()))
}
