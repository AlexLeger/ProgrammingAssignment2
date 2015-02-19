## The goal of these two functions is to provide a way to cache the 
## inverse of a matrix once it has been computed for the first time.
## Then, if the inverse of this matrix is called again, it will be
## retrieved from the cache rather than computed again, which can be
## very time consuming.

## This function will take a matrix and return a list of four
## functions.

makeCacheMatrix <- function(x = matrix()) {
      inv <- NULL
      set <- function(y) {
            x <<- y
            inv <<- NULL
      }
      get <- function() x
      setinv <- function(inverse) inv <<- inverse
      getinv <- function() inv
      list(set = set, get = get,
           setinv = setinv,
           getinv = getinv)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
      inv <- x$getinv()
      if(!is.null(inv)) {
            message("getting cached data")
            return(inv)
      }
      data <- x$get()
      inv <- solve(data, ...)
      x$setinv(inv)
      inv
}
