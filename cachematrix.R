# ProgrammingAssignment2
#
# Tested using:
# > source('./cachematrix.R')
# > m<-matrix(runif(4000000,1,1000000),2000,2000)
# > cm<-makeCacheMatrix(m)
# > inv<-cacheSolve(cm)
# calculating inverse matrix
# > inv<-cacheSolve(cm)
# getting cached data
# > 
#
# The first function, makeCacheMatrix creates a special "matrix",
# which is really a list containing a function to
# set the value of the matrix
# get the value of the matrix
# set the value of the inverse matrix
# get the value of the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
  
  # Initialize inverse to null
  inverse <- NULL
  
  # stores the matrix to be inverted into x and resets the inverse to NULL
  set <- function(y) {
    x <<- y
    inverse <<- NULL
  }
  
  # returns the matrix
  get <- function() x
  
  # stores the inverted matrix into inverse
  setinverse <- function(i) inverse <<- i
  
  # returns the inverse matrix (or NULL if it hasn't been calculated yet)
  getinverse <- function() inverse
  
  # makeCacheMatrix return value - a list of methods
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

# This function computes the inverse of the special "matrix"
# returned by makeCacheMatrix above. If the inverse has already
# been calculated (and the matrix has not changed), then the
# cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  
  # retrieve the inverse matrix from the cacheMatrix object
  i <- x$getinverse()
  
  # if the inverse matrix is not NULL return it and exit the function
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  
  message("calculating inverse matrix")
  
  # retrieve the matrix from the cacheMatrix object
  data <- x$get()
  
  # invert the matrix
  i <- solve(data, ...)
  
  # store the inverted matrix into the cacheMatrix object
  x$setinverse(i)
  
  # return the inverted matrix
  i
}
