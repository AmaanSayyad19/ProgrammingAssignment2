makeCacheMatrix <- function(x = matrix()) {
  # Initialize the inverse to NULL
  inverse <- NULL
  
  # Method to set the matrix
  set <- function(y) {
    x <<- y
    inverse <<- NULL  # Reset the inverse when the matrix changes
  }
  
  # Method to get the matrix
  get <- function() {
    x
  }
  
  # Method to set the inverse
  setInverse <- function(inverse) {
    inverse <<- inverse
  }
  
  # Method to get the inverse
  getInverse <- function() {
    inverse
  }
  
  # Return a list of the methods
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}



cacheSolve <- function(x, ...) {
  # Get the cached inverse
  inverse <- x$getInverse()
  
  # If the inverse is NULL, calculate it
  if (is.null(inverse)) {
    mat <- x$get()
    inverse <- solve(mat, ...)
    x$setInverse(inverse)  # Cache the inverse
  }
  
  # Return the inverse
  inverse
}

