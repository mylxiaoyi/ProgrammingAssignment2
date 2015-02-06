## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  
  ## set the value of the matrix
  set <- function(y) {
    if (!identical(x,y)) {
      x <<- y
      inv <<- NULL
    }
  }
  
  ## get the value of the matrix
  get <- function() x
  
  ## set the inverse value of the matrix
  setinv <- function(inverse) inv <<- inverse
  
  ## get the inverse value of the matrix
  getinv <- function() inv
  
  list(set=set, get=get,
       setinv=setinv, getinv=getinv)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  
  ## get the inverse value of the matrix
  inv <- x$getinv()
  
  ## check to determine whether it is cached
  if (!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  
  ## get the value of the matrix
  data <- x$get()
  
  ## compute the inverse value of the matrix
  inv <- solve(data)
  
  ## set the inverse value of the matrix to cache
  x$setinv(inv)
  
  ## return the computed inverse value
  inv
}
