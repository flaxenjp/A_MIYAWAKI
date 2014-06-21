## Below are 2 functions that are used to create a special
## object that stores a numeric matrix and caches its inverse matrix.

## "makeCacheMatrix" creates a list　containing a function to 
## "set the value of the matrix", "get the value of the matrix",
## "set the value of the solve", "get the value of the solve"

makeCacheMatrix <- function(x = matrix()) {
  s <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  
  setsolve <- function(solve) s <<- solve
  
  getsolve <- function() s
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
}


## the following function calculates a inverse matrix of matrix 'x'.
## It first checks to see if the inverse matrix has already been calculated.
## If so, it gets the inverse matrix from the cache and skip the computation.
## Otherwise, it calculates the inverse matrix of the data and sets the
## inverse matrix in the cache via the setsolve function.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  
  s <- x$getsolve()
  if(!is.null(s)) {
    message("getting cached data")
    return(s)
  } 
  
  data <- x$get()
  s <- solve(data, ...)
  x$setsolve(s)
  s
}
