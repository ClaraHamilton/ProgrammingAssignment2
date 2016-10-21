## This function creates a cache of Matrix Inverses
## cacheSolve then will use the cache to return the 
## Inverse of the matrix

## This function creates a cache of matrix Inverses

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y){
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinv <- function(inverse) i <<- inverse
  getinv <- function () i
  list(set= set, get= get, 
       setinv = setinv, getinv = getinv)
}


## This function returns a the Inverse of a function
## using the cache of Matir's created above

cacheSolve <- function(x, ...) {
  m <- x$getinv()
  if(!is.null(m)){
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinv(m)
  m
}
