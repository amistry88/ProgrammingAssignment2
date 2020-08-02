## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  invrs <- NULL
  set <- function(y) {
    x <<- y
    invrs <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) invrs <<-inverse
  getinverse <- function() invrs
list(set = set, get = get,
     setinverse = setinverse,
     getinverse = getinverse)
}

## Write a short comment describing this function
## Return a matrix that is the inverse of 'x'
cacheSolve <- function(x, ...) {
  invrs <- x$getinverse()
  if (!is.null(invrs)) {
    message("getting cached matrix")
    return(invrs)
  }
  matrix <- x$get()
  invrs <- solve(matrix, ...)
  x$setinverse(invrs)
  invrs
  
}

A<-matrix(c(2,3,6,4),2,2)
A
Ainv<-makeCacheMatrix(A)
cacheSolve(Ainv)
cacheSolve(Ainv)
