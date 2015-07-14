## A pair of functions written to satisfy requirements for week 3 programming assignment
## in Coursera's R-Programming course.

## makeCacheMatrix is a constructor function for a special type of matrix which caches
## its inverse so that it only needs to be calculated once.
## x is assumed to be invertible; there is no error checking to make sure that this
## is so. The function does not return a matrix, but a list of methods, which allow
## one to set/get the matrix, and to set/get the matrix inverse.

makeCacheMatrix <- function(x = matrix()) {
     xInv <- NULL
     set <- function(y) {
          x <<- y
          xInv <<- NULL
     }
     get <- function() x
     setInverse <- function(inv) xInv <<- inv
     getInverse <- function() xInv
     list(set = set, get = get,
          setInverse = setInverse,
          getInverse = getInverse)
}


## cacheSolve takes a list of methods from a call to makeCacheMatrix, a matrix which
## caches its inverse, and returns the inverse of the matrix. If the inverse has
## been calculated before, then a cached version of the inverse is returned, otherwise
## the function calculates the inverse and caches it before returning it.

cacheSolve <- function(x, ...) {
     ## Return a matrix that is the inverse of 'x'
     xInv <- x$getInverse()
     if(!is.null(xInv)) {
          message("getting cached data")
          return(xInv)
     }
     data <- x$get()
     xInv <- solve(data, ...)
     x$setInverse(xInv)
     xInv
}
