## write a pair of functions that cache the inverse of a matrix.
## named, makeCacheMatrix() & cacheSolve()

## This function creates a special "matrix" object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
  # stores the inverse value
  inverse <- NULL
  
  # sets the matrix variable and reset the inverse variable
  set <- function(y) {
    x <<- y
    inverse <<- NULL
  }
  
  # gets the value of the matrix variable
  get <- function() x
  
  # calculates the inverse of the matrix
  setInverse <- function(solve) inverse <<- solve
  
  # gets the value of the inverse of the matrix
  getInverse <- function() inverse
  
  # returns a list of the 4 functions that just got defined
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
  

}

## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
## If the inverse has already been calculated (and the matrix has not changed), 
## then cacheSolve should retrieve the inverse from the cache
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  ## if its cache value is not set to NULL
  inverse <- x$getInverse()
  if(!is.null(inverse)) {
    message("getting cached data")
    return(inverse)
  }
  
  ## if it's a new matrix or there is no cache saved for the matrix
  ## then calculate the inverse of the matrix
  
  # First retrieve the original matrix
  data <- x$get()
  # Then calculate its inverse
  inverse <- solve(data, ...)
  # set the newly calculated inverse value to the cache
  x$setInverse(inverse)
  # return the inverse value
  inverse
}
