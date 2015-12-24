## Put comments here that give an overall description of what your
## functions do:
## These functions are used to create a special object that 
## stores a matrix and caches its inverse

## Write a short comment describing this function:
## The function creates the object that stores matrix and caches its inverse

makeCacheMatrix <- function(x = matrix()) {
      i <- NULL
      set <- function(y) {
            x <<- y
            i <<- NULL
      }
      get <- function() x
      cacheinv <- function(inverse) i <<- inverse
      getinv <- function() i
      list(set = set, get = get, cacheinv = cacheinv, getinv = getinv)

}


## Write a short comment describing this function:
## Thus function calculates the inverse of our matrix. If it already exists the
## function retrieve it from the cache. If the matrix is not invertible the function
## returns the message "matrix is not invertable".

cacheSolve <- function(x, ...) {
      ## Return a matrix that is the inverse of 'x'
      i <- x$getinv()
      if (!is.null(i)) {
            message("getting cached data")
            return(i)
      }
      dat <- x$get()
      if (det(dat) == 0) {
         message("matrix is not invertable")
      }
      else {i <- solve(dat, ...)
      x$cacheinv(i)
      i
      }
}
