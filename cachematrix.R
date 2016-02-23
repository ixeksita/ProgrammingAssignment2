## makeCacheMatrix creates "matrix" which is essentialy
##a list for the following functions:
## A) set the matrix values, B) get the value of the  matrix
## C) set the value of the inverse of the matrix
## d) get the value of the inverse of the matrix
## cacheSolve is used to calculate the inverse of the "matrix"
## created by makeCacheMatrix. Note that this checks if the inverse of 
#such matrix has been previously calculated, if so, it returns
## the previsouly cached value, otherwise it computes the inverse
## and uses setinv to set the inverse value in the cache

## This function creates a "matrix" capable to cache its inverse

makeCacheMatrix <- function(x=matrix() ) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(solve) inv <<- solve
  getinv <- function() inv
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
  
}


## This function computes the inverse of the matrix returnes by makeCacheMatrix. 
## Had the inverse previsouly been computed it retrieves the inverse from the cache
##(NOTE: the matrix should remain unchanged)
#otherwive, it proceeds to compute the inverse

cacheSolve <- function(x, ...) {
  inv <- x$getinv()
  if(!is.null(inv)) {
    message("Getting  previously cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinv(inv)
  inv
}
