# Theses functions are used to cache the inverse of a matrix.

# This first function called makeCacheMatrix creates a list containing a function to
# set the value of the matrix
# get the value of the matrix
# set the value of inverse of the matrix
# get the value of inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  invm <- NULL
  setm <- function(y) {
    x <<- y
    invm <<- NULL
  }
  getm <- function() x
  setinv <- function(inverse) invm <<- inverse
  getinv <- function() invm
  list(setm=setm, getm=getm, setinv=setinv, getinv=getinv)
}


# The following function returns the inverse of the matrix created with the above function.
# However, it first checks to see if the inverse has already been calculated.
# If so, it gets the result and skips the computation. 
# If not, it computes the inverse, sets the value in the cache via setinv function.

cacheSolve <- function(x, ...) {
  invm <- x$getinv()
  if(!is.null(invm)) {
    return(invm)
  }
  data <- x$getm()
  invm <- solve(data)
  x$setinv(invm)
  invm
}
