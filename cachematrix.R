## I made two functions. 
## One is to create Matrix, and the other is to obtain an inverse value for the matrix.

## I defined four functions within the makeCacheMatrix(). set(), get(), setinverse(), getinverse().
## First step is the initialization of two objects, x and i.
## i is, the value of inverse.

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) i <<- solve
  getinverse <- function() i
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## cacheSolve is a function to obtain an inverse value.
## This function attempts to retrieve a inverse value from the object passed in as the argument using getinverse.
## Then it checks to see whether the result is NULL.

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
