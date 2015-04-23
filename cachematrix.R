## Programming Assignment 2 for Cousera "R Programming" course
##
## Demonstrating how we can do our calculations faster by caching results for
## time-intensive computation, for example inverse of a matrix in our case here.

## Helper function to check if given matrix is square matrix (nxn)

isSquareMat <- function( m ) {
  return (nrow(m) == ncol(m))
}

## Helper function to check if given matrices are equal

isEqualMat <- function(m1, m2)
  is.matrix(m1) && is.matrix(m2) && dim(m1) == dim(m2) && all(m1 == m2)


## makeCacheMatrix creates a special "matrix", which is really a list containing
## a function to:
## -set the value of the matrix
## -get the value of the matrix
## -set the value of the inverse
## -get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL
  if( !isSquareMat(x) ) warning("Given non-square matrix...expecting a square matrix!")
  set <- function(y) {    
    if( !isEqualMat(x, y) ) {
      if( !isSquareMat(y) ) warning("Given non-square matrix...expecting a square matrix!")
      x <<- y
      inverse <<- NULL
    }
  }
  get <- function() x
  setinverse <- function(v) inverse <<- v
  getinverse <- function() inverse
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)  
}


## The following function calculates the inverse of the special "matrix"
## created with the "makeCacheMatrix" function. However, it first checks
## to see if the inverse has already been calculated. If so, it gets the
## inverse from the cache and skips the computation. Otherwise, it calculates
## the inverse of the data and sets the value of the inverse in the cache via
## the "setinverse" function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  v <- x$getinverse()
  if(!is.null(v)) {
    message("getting cached data")
    return(v)
  }
  data <- x$get()
  if(!isSquareMat(data)) stop("Inverse is only valid for a square matrix!")
  v <- solve(data, ...)
  x$setinverse(v)
  v  
}
