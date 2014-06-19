## To make the costly matrix inversion computation, special functions have been created.
## They take advantage of lexical scoping of R language to persist inversion of the matrix.
## It is assumed that the matrix supplied is always invertible.

## makeCacheMatrix is a function which creates a special matrix object that can cache 
## its inverse by caching it in the parent env. object

makeCacheMatrix <- function(x = matrix()) {
  
  inv <- NULL
  
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  
  get <- function() x
  
  setinv <- function(inverse) inv <<- inverse
  
  getinv <- function() inv
  
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
  
}


## cacheSolve function makes use of the makeCacheMatrix to calculate the inverse of the
## matrix. If the inverse of the matrix already calculated, it returns the cached data.
## otherwise, calculate the inverse and sets it back to the special matrix object.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getinv()
  
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  
  data <- x$get()
  
  m <- solve(data)
  
  x$setinv(m)
  
  m        
}