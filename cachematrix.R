## The goal of these two functions is to provide a way to cache the 
## inverse of a matrix once it has been computed for the first time.
## Then, if the inverse of this matrix is called again, it will be
## retrieved from the cache rather than computed again, which can be
## very time consuming.

## This function will take a matrix and return a list of four
## functions, allowing to access both the original matrix and
## its inverse.



makeCacheMatrix <- function(x = matrix()) {
      inv <- NULL
      set <- function(y) { ## The set function allows the original matrix to be modified
            x <<- y
            inv <<- NULL ## Since the original matrix has been modified, the inverse that had been computed is no longer valid
      }
      get <- function() x
      setinv <- function(inverse) inv <<- inverse
      getinv <- function() inv
      list(set = set, get = get,
           setinv = setinv,
           getinv = getinv)
}


## This function takes a special "list" such as the one created by 
## the makeCacheMatrix function, and does the following :
## 1) Checks whether the inverse of the matrix has already been computed
## 2) If so, returns this inverse with the message "getting cached matrix"
## 3) If not, it computes the inverse of the matrix, stores it using the setinv function, then returns it.

cacheSolve <- function(x, ...) {
      inv <- x$getinv()
      if(!is.null(inv)) {
            message("getting cached matrix")
            return(inv)
      }
      data <- x$get()
      inv <- solve(data, ...)
      x$setinv(inv)
      inv
}
