## These functions work together to enable a matrix to persistently maintain
##   the information about it's inverse matrix, to avoid time-consuming recalculation

## makeCacheMatrix() takes a matrix as it's argument and creates a data structure
##   with variables to hold the matrix and it's inverse
##   and functions to get and set both.   

makeCacheMatrix <- function(x = matrix()) {
  # Take the matrix x as a parameter.  This is the primary matrix, which will
  #    be retained in the environment of the 'CacheMatrix' data structure returned 
  #    by this function.
  
  # Initialize the variable to hold inverse matrix.  This variable will be retained
  #    in the environment of the 'CacheMatrix' data structure returned by this function.
  invmatrix <- NULL
  # Create a function called 'set' within the 'CacheMatrix' data structure, which can 
  #    be passed a matrix as a parameter and which will set the value of the primary
  #    matrix to this value.  Also resets the inverse matrix to Null, since any
  #    change to the primary matrix invalidates the old stored inverse matrix.
  set <- function(y) {
    x <<- y
    invmatrix <<- NULL
  }
  # Create a function called 'get' within the 'CacheMatrix' data structure, which 
  #    doesn't take any arguments but simply returns the primary matrix
  get <- function() x
  # Create a function called 'setInv' within 'CacheMatrix', which takes a matrix as 
  #   a parameter and sets the stored inverse matrix to that parameter.  Used by cacheSolve
  setInv <- function(newmatrix) invmatrix <<- newmatrix
  # Create a function called 'getInv' within 'CacheMatrix', which takes no parameters
  #   but simply returns the inverse matrix 
  getInv <- function() invmatrix
  
  # Instantiate the CacheMatrix object by returning a list containing the four functions
  #   which act as the interface to CacheMatrix
  list(set = set, get = get,
       setInv = setInv,
       getInv = getInv)
}


## cacheSolve takes a 'CacheMatrix' data structure created by the
##     makeCacheMatrix function and operates on it to
##      1. calculate the inverse matrix using solve() if it's not already in the CacheMatrix instance
##      2. Get the pre-calculated inverse if it's already available.

cacheSolve <- function(x, ...) {
  ## Take x as a parameter, which is a CacheMatrix instance
  ## Return a matrix that is the inverse of 'x'
  
  # Use the CacheMatrix instance's own getInv function ("CacheMatrix.getInv") to get the inverse matrix
  minverse <- x$getInv()
  # If the inverse matrix is available (it's not null) provide a message saying
  #  we're using the cached data and return to leave the function.
  if(!is.null(minverse)) {
    message("getting cached data")
    return(minverse)
  }
  # If we didn't leave the function in the step above, then the inverse matrix is
  #   not available, so we need to calculate it.
  # Use "CacheMatrix.get()" to get the original matrix, which is our source data for
  #   our inversion calcualtion, and store it in "data"
  data <- x$get()
  # Use solve() to get the inverse of the matrix in "data".  Store it in "minverse"
  minverse <- solve(data)
  # Use "CacheMatrix.setInv()" to store the result back in the CacheMatrix instance
  x$setInv(minverse)
  # Return the inverse so that the user sees it at the command line.  It should also now
  #   be available directly from the CacheMatrix instance.
  minverse
}
