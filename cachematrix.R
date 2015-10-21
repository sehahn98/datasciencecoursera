## makeCacheMatrix and cacheSolve are functions that work 
## together to save the inverse of a matrix and recall it
## if you ask for the inverse of that matrix again.

## makeCacheMatrix saves the inverse of a matrix in memory
## where it can easily be called back. If you run
## makeCacheMatrix again, you will overwrite the previous
## matrix. 

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setsolve <- function(solve) m <<- solve
  getsolve <- function() m
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)

}


## cacheSolve ckecks to see if there is an inverted matrix
## in memory that corresponds to the matrix being passed
## to the function. If there is, it returns that inverted
## matrix. If there is no cached matrix, it will calculate
## the inverse and return it.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getsolve()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setsolve(m)
  m
}
