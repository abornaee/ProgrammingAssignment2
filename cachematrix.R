## There is two functions here: 1. makeCacheMatrix , 2.cacheSolve. 
##There is a short description before the header of each function 

## makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  setMatrix <- function (y) {
    x <<- y
    m <<- NULL
  }
  getMatrix <- function() x    
  setInverse <- function(inverse) m <<- inverse 
  getInverse <- function() m
  
  list(setMatrix = setMatrix, getMatrix = getMatrix,
       setInverse = setInverse,
       getInverse = getInverse)
}


##cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
  m <- x$getInverse()
  if(!is.null(m)){
    message("Getting chached inverse matrix")
    return(m)
  }
  matr <- x$getMatrix()  
  m <- solve(matr)
  x$setInverse(m)
  m  
}
