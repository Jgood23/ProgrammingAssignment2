## The functions below have been designed to create an object that stores a matrix,
## and caches its inverse, removing the need to compute it repeatedly.

## The following function is designed to create a list of four functions
## capable of storing and returning the inverted matrix

makecachematrix <- function(x = matrix()){
  inv <- NULL
  set <- function(y){
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  
## List produced for the four functions
  
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}

## This function computes the inverse of the matrix produced by makecachematrix
## The inverse is retrieved from the cache if it has already been calculated and
## the matrix is unchanged.

cachesolve <- function(x, ...){
  
## This function returns a matrix that is an inverse of x
  
  inv <- x$getinverse()
  if (!is.null(inv)){
    return(inv)
  }
  mat <- x$get()
  inv <- solve(mat, ...)
  x$setinverse(inv)
  inv
}