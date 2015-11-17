# Solution to programming assignment 2 from the Coursera R programming course.
# Author: Dimitri Hendriks
# Date: 2015-11-17
#
# Matrix inversion is usually a costly computation 
# and there may be some benefit to caching the inverse of a matrix 
# rather than computing it repeatedly. 
# 
# Below we give a pair of functions that cache the inverse of a matrix, namely:
# 1. makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
# 2. cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix. 
#    If the inverse has already been calculated (and the matrix has not changed), 
#    then cacheSolve should retrieve the inverse from the cache.

# Example usage:
# > x <- matrix(rnorm(25, 10), 5, 5)          // create a(n invertible) 5x5-matrix x
# > cx <- makeCacheMatrix(x)                  // create the special matrix
# > cx$get()                                  // return the matrix x
# > cacheSolve(cx)                            // return the inverse of x
# > cacheSolve(cx)                            // 2nd call returns the cached inverse


# The function makeCacheMatrix creates a special "matrix", 
# a list containing a function to
# 
# 1. set the value of the matrix
# 2. get the value of the matrix
# 3. set the value of the inverse matrix
# 4. get the value of the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y) {
      x <<- y
      i <<- NULL
    }
    get <- function() x
    setinv <- function(inv) i <<- inv
    getinv <- function() i
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}

# The following function calculates the inverse of the special "matrix" 
# created with the function makeCacheMatrix above. 
# It first checks to see if the inverse has already been calculated. 
# If so, it gets the inverse from the cache and skips the computation. 
# Otherwise, it calculates the inverse of the data and sets the value of the inverse 
# in the cache via the setinv function.
# We assume that the matrix supplied is always invertible.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  i <- x$getinv()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data) # this computes the inverse of data (which we assume is an invertible matrix)
  x$setinv(i)
  i
}
