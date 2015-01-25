## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  ## The function creates a special "matrix" object that can cache its inverse
  s <- NULL
  ## set the value of the matrix
  set <- function(y) {
    x <<- y
    ## flush the cache when matrix changed via set function
    s <<- NULL
  }
  ## get the value of the matrix
  get <- function() x
  ## set the value of the inverse of the matrix
  setsolve <- function(solve) s <<- solve
  ## get the value of the inverse of the matrix
  getsolve <- function() s
  ## return special "matrix", which is really a list of above functions
  list(set = set, get = get,
       setsolve = setsolve, getsolve = getsolve)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
  ## The function returns inverse of the 'x':
  ## - either cached one (if exists)
  ## - or calculated one (and cache it)
  
  ## check if there is cached inverse of the "matrix" 'x'
  s <- x$getsolve()
  if (!is.null(s)) {
    ## return cached inverse of the 'x' if there is some
    message("Getting cached data")
    return(s)
  }
  ## calculate inverse of the 'x' if there is nothing cached
  matrix <- x$get()
  s <- solve(matrix)
  ## cache calculated inverse of the 'x'
  x$setsolve(s)
  ## return a matrix that is the inverse of 'x'
  s
}
