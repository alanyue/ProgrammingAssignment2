## creates a special "matrix" object that can cache its inverse
## This "matrix" has 4 attributes : 
##    - get() : return the matrix
##    - set() : set the matrix
##    - getInverse() : return the cached inverse matrix
##    - setInverse() : set the inverse matrix internally

makeCacheMatrix <- function(x = matrix()) {

  ## internal data to store the cached inverse matrix
  inveredM <- NULL
  
  ## set function to assign the given matrix, and reset
  ## the inveredM to NULL to indicate new matrix is received
  set <- function(y){
    x <<- y
    inveredM <<- NULL
  }
  
  ## get function to return the matrix
  get <- function() x
  
  ## set the calculated inverse matrix into the cache
  setInverse <- function(m) inveredM <<- m
  
  ## return the cached inverse matrix
  getInverse <- function() inveredM
  
  ## return the "matrix" object
  list( set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## This function computes the inverse of the special "matrix"
## returned by makeCacheMatrix.  If the inverse has already been
## calculated (and the matrix has not changed), then the 
## cacheSolve should retrieve the inverse from the cache.

## Return a matrix that is the inverse of 'x'
cacheSolve <- function(x, ...) {
  ## get the cached copy of the inverse matrix
  i <- x$getInverse()
  
  ## check whether the matrix is unchanged. If yes,
  ## return the cached copy
  if(! is.null(i)){
    message( "getting cached data")
    return(i)
  }
  
  ## need to recacluate the inverse matrix
  m <- x$get()
  i <- solve(m, ...)
  
  ## put back the calculated inverse matrix into the cache
  x$setInverse(i)
  
  ## return the inverse matrix
  i
}
