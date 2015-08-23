## Assignment 2 - cachematrix

##The makeCacheMatrix function creates a special "matrix" 
## which is really a list containing a functions to:
## set the matrix
## get the matrix
## set the inverse matrix
## get the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y){
    x <<- y
    inv <<- NULL
  }
  
  get <- function() x
  
  setInverse <- function(inverse) inv <<- inverse
  
  getInverse <- function() inv
  
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)  
  
}

## The cacheSolve function checks if the inverse matrix has been stored, if not it uses solve() to 
## inverse it and then stores it.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'.
  inv <- x$getInverse()
  
  if(!is.null(inv)){
    message("getting cached data")
    return(inv)
  }
  
  data <- x$get()
  inv <- solve(data, ...)
  x$setInverse(inv)
  inv
}
