## R Programming - Programming Assignment 2

## This function creates special vector that caches the inverse of the matrix object
## It achieves the caching by creating the vector as a list of of 'get' and 'set' functions
## for the matrix and its inverse along with the scoping manipulation via <<-

makeCacheMatrix <- function(x = matrix()) {
  Inv <- NULL # Inverse begins as NULL
  
  # Set the original object to be stored and later inverted
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  
  # Returns original object
  get <- function() x
  
  # Set and Get functions for inverse, similar to above
  setInverse <- function(inverse) inv <<- inverse
  getInverse <- function() m
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## This function will retrieve the original matrix object, calculate its inverse,
## and cache the inverse in the CacheMatrix object.
## If the cache already exists, it will return the current inverse.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  
  # Check if cached inverse object already exists
  inv <- x$getInverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  
  # Get original matrix object
  data <- x$get()
  
  # Calculate inverse
  inv <- solve(data)
  
  # Cache inverse using CacheMatrix
  x$setInverse(inv)
  inv
}



