## function(makeCacheMatrix) can created a kind of matrix, whose inverse can be saved in cache to acceralate the calculation
## functions(cacheSolve) can calculate the inverse of matrix created by function(makeCacheMatrix)

## makeCacheMatrix creates a list that containing 4 functions to achieve:
## 1. set a matrix / 2. get that matrix / 3. set the inverse that matrix / 4. get the inverse of that matrix
makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(s) i <<- s
  getinverse <- function() i
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## This function computes the inverse of the matrix returned by makeCacheMatrix. 
## If the inverse has already been calculated then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        i <- x$getinverse()
        if(!is.null(i)) {
          message("getting inverse from cached data")
          return(i)
        }
        data <- x$get()
        i <- solve(data, ...)
        x$setinverse(i)
        i
}
