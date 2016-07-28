## makeCacheMatrix creates a "special matrix" object that can cache its own
## inverse.
## The special matrix is given to cacheSolve, which will -  when called the
## first time - compute the inversed matrix and store it in the special matrix.
## All subsequent calls of cacheSolve will fetch the inversed matrix from the
## cache within the special matrix.


## Creates a "special matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  cached_inverse <- NULL
  set <- function(y) {
    x <<- y
    cached_inverse <<- NULL
  }
  get <- function() x
  setinverse <- function(inversed_matrix) cached_inverse <<- inversed_matrix
  getinverse <- function() cached_inverse
  list(set = set,
       get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## cacheSolve will compute the inverse of a "special matrix" created by
## makeCacheMatrix. It will cache the inverted matrix and respond to
## subsequent requests to calculate an inverted matrix with the cached version
## to deliver a faster response.

cacheSolve <- function(x, ...) {
  inverse <- x$getinverse()
  if(!is.null(inverse)) {
    message("getting cached data")
    return(inverse)
  }
  data <- x$get()
  inverse <- solve(data)
  x$setinverse(inverse)
  inverse
}
