## makeCacheMatrix function is used to create a matrix object that can cache its inverse.
## cacheSolve function returns the inverse of the matrix created from makeCacheMatrix 
## function. The inverse is retrieved from cache if it has already been calculated earlier.

## makeCacheMatrix function takes a matrix as its parameter and returns the corresponding 
## matrix object which can store a cached version of its inverese.

makeCacheMatrix <- function(x = matrix()) {
   inverse <- NULL
  set <- function(y) {
    x <<- y
    inverse <<- NULL
  }
  get <- function() x
  setinv <- function(inv) inverse <<- inv
  getinv <- function() inverse
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## cacheSolve function takes the above matrix object as its parameter , computes its 
## inverses and caches the inverse for later requirement.

cacheSolve <- function(x, ...) {
       inverse <- x$getinv()
  if(!is.null(inverse)) {
    message("getting cached data")
    return(inverse)
  }
  data <- x$get()
  inverse <- solve(data, ...)
  x$setinv(inverse)
  inverse ## Return a matrix that is the inverse of 'x'
}
