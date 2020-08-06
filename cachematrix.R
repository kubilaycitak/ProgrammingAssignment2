## The code's aim is to cache the inverse of a matrix in order to be able to
## use the timing advantage of caching while calculating matrices.


## Preparing an inverse-cacheable matrix.
makeCacheMatrix <- function(mtr = matrix()) {
  
  # Setting the inverse NULL for now.
  invrs <- NULL
  set <- function(y) {
    
    # `<<-` assigns a value to an object in an environment 
    # different from the current environment. 
    mtr <<- y
    invrs <<- NULL
  }
  
  # Get/Set operations of inverse.
  get <- function() mtr
  setInverse <- function(inverse) invrs <<- inverse
  getInverse <- function() invrs
  list(set = set,
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)
  

}


## Returns a matrix that is the inverse of 'mtr'
cacheSolve <- function(mtr, ...) {
  
  # Getting inversed matrix
  inv = mtr$getInverse()
  
  # If it's already calculated, return the value.
  if (!is.null(inv)){
    message("getting cached data")
    return(inv)
  }
  
  # If it's not calculated, calculate it and cache it.
  mat.data = mtr$get()
  inv = solve(mat.data, ...)
  mtr$setInverse(inv)
  
  return(inv)
}
