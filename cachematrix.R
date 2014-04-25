## creates a special "matrix" object that can cache its inverse
## This "matrix" has 4 attributes : 
##    - get() : return the matrix
##    - set() : set the matrix
##    - getInverse() : return the cached inverse matrix
##    - setInverse() : set the inverse matrix internally

makeCacheMatrix <- function(x = matrix()) {
  inveredM <- NULL
  
  set <- function(y){
    x <<- y
    inveredM <<- NULL
  }
  
  get <- function() x
  
  setInverse <- function(m) inveredM <<- m
  getInverse <- function() inveredM
  
  list( set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## This function computes the inverse of the special "matrix"
## returned by makeCacheMatrix.  If the inverse has already been
## calculated (and the matrix has not changed), then the 
## cacheSolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  i <- x$getInverse()
  if(! is.null(i)){
    message( "getting cached data")
    return(i)
  }
  
  m <- x$get()
  i <- solve(m, ...)
  x$setInverse(i)
  i
}
