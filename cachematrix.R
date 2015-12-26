## makeCacheMatrix creates special matrix that can be cached with 2nd function

## makeCacheMatrix contains 4 functions, getmatrix, setmatrix, getinv, and setinv
## these functions perform simple cacheing operations

makeCacheMatrix <- function(x = matrix()) {
  i <<- NULL
  setmatrix <- function (y){
    x <<- y
    i <<- NULL
  }
  getmatrix <- function() x
  setinv <- function (inv) i <<- inv
  getinv <- function () i
  list(setmatrix=setmatrix, getmatrix=getmatrix, setinv=setinv, getinv=getinv)
}

## this function will find cache or solve if none available
##arguments must have been put through makeCacheMatrix function first

cacheSolve <- function(x, ...) {
  i <- x$getinv()
  if(!is.null(i)){
    message("getting cached data")
    return(i)
  }
  data <- x$getmatrix()
  i <- solve(data, ...)
  x$setinv(i)
  i
}

