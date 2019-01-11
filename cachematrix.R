## Put comments here that give an overall description of what your
## functions do

## This function creates a "matrix" object that can cache its inverse.

## Write a short comment describing this function
## This function creates the matrix object and stores the origional matrix
makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(j) {
    i <<- j
  }
  getinverse <- function() i
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}

## Write a short comment describing this function
## This function calculates and stores the inverse of a matrix object
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  i <- x$getinverse()
  if(!is.null(i)) {
    message("Getting cached inverse")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i
}
