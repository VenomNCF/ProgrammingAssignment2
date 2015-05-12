## The following two functions can be used to cache the inverse of a matrix.

## This function creates a special "matrix" that can cache its inverse.
makeCacheMatrix <- function ( x = matrix() ) {
  inverse <- NULL;
  
  set <- function(y) {  
  x <<- y
  inverse <<- NULL
  }
  
  get <- function()
  x;
  
  setinverse <- function(inv)
  inverse <<- inv
  
  getinverse <- function()
  inverse
  list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


##  This function computes the inverse of the special "matrix" returned by 
##  MakeCacheMatrix above.

cacheSolve <- function(x, ...) {
  inverse <- x$getinverse();
  if(!is.null(inverse)) {
  return(inverse)
  }
  data <- x$get();
  inverse<-solve(data)
  x$setinverse(inverse)
  inverse
}