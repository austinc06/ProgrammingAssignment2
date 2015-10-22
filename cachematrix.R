## Put comments here that give an overall description of what your
## functions do

## makeCacheMatrix creates a special Matrix list to store a matrix and its inverse
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  
  ## Set function. Sets new matrix and resets inverse to NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  
  ## Get function. Return current stored matrix
  get <- function() x
  
  ## SetInverse function. Sets the inverse to calculated input.
  setInverse <- function(inversed) inv <<- inversed
  
  ## GetInverse function. Return inverse
  getInverse <- function() inv
  
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}

## cacheSolve returns the cached Inverse matrix or otherwise calculates and 
##stores the inverse matrix of a cacheMatrix
cacheSolve <- function(x, ...) {
  ## Call getInverse function from matrix x
  inv <- x$getInverse()
  
  ## If inverse is already cached, just return
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  
  ## If inverse is not cached, calculate and set
  data <- x$get()
  
  ## Check if matrix is square
  if (dim(data)[1] != dim(data)[2]) {
    stop("Stored Matrix is not Square. Cannot take inverse.")
  }
  else {
    inv <- solve(data, ...)
    x$setInverse(inv)
    inv
  }
}