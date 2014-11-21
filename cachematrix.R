## These 2 functions compute the inverse of a given matrix (assuming it's invertible)
## and cache the calculation after the first trial. Next uses of the inverse matrix will refer
## to this cached calculation

## This function initiates the instance of the cacheable inverse matrix

makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL
  set <- function(y) {
    x <<- y
    inverse <<- NULL
  }
  get <- function () x
  setinverse <- function(matrix_inv) inverse <<-matrix_inv
  getinverse <- function() inverse
  list (set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}

## This function will calculate the matrix inverse

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inverse_local <- x$getinverse()
  if (!is.null(inverse_local)) {
    message("getting cached data")
  } else {
    dones <- x$get()
    inverse_local <- solve(dones)
    x$setinverse(inverse_local)
  }
  inverse_local
}