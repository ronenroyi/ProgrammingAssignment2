## this is an implementation of an enhanced matrix objet, which can cache its inverse.
## the forst function creates the object based on an input of a rgeular matrix.
## the second one returns computes the inverse, only if necessary. If already cahced, the cached value 
## is returned, and the extra computation is saved.


## this methids takes a matrix x and makes a new, enhanced, matrix object that is able to store a cached value of its inverse
## The function does not enforce the the matrix is invertible  - this will be done in the cacheSolve method
##(no such requirement, and doing this would limit code reusability).
makeCacheMatrix <- function(x = matrix()) {
  # will store the inverse of the matrix once cmputed.
  xinverse <- NULL 
  #setting a new value for the matrix and resetting the inverse to null
  set <- function(y) {
    x <<- y
    inverse <<- NULL
  }
  # returns the matrix
  get <- function() x
  #stores the inverse given in the input into xinverse
  setinverse <- function(inverse) xinverse <<- inverse
  #returns the inverse from the cache; null if not computed.
  getinverse <- function() xinverse
  #the list of methods that have been created for this object is returned
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## This function takes a new matrix object; and tries to retrieve the inverse from the cache.
## Only if null it computes the inverse of the matrix and returns it.
## if thr matrix is not inversible, an error is printed and null is returned instead of the inverse.

cacheSolve <- function(x, ...) {
  i <- x$getinverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i
}

