## This code consists of two functions used to
##   calculate the inverse of a matrix if the inverse
##   has not been previously calculated

## the function makeCacheMatrix creates a list of
##  functions that cache a matrix, get the cached
##  matrix, compute the inverse and cache it
## and get the inverse from the cache

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinv <- function(solve) m <<- solve
  getinv <- function() m
  
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## the function cacheSolve returns the inverse matrix
##  of matrix 'x'.  It first checks if a value has
##  been previously cached, and if so returns that
##  value.  Otherwise the function calculates the
##  inverse, caches it and returns it

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getinv()
  if(!ls.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinv(m)
  m
  
}
