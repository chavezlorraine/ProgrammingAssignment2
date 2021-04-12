## A couples of functions that cache the inverse of a matrix

## Makes a special matrix object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  j <- NULL
  z <- NULL
  set <- function(y){
    x <<- y
    j <<- NULL
    z <<- NULL
  }
  get <- function()x
  setInverse <- function(inverse) j <<- inverse
  getInverse <- function() j 
  setInverse <- function(inverse) z <<- inverse
  getInverse <- function() z
  list(set = set, get = get, 
       setInverse = setInverse, 
       getInverse = getInverse)
}


## Compute the inverse of the special matrix returned by "makeCacheMatrix"
## If the inverse has already been calculated (and the matrix has not
## changed), then the "cachesolve" should retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  j <- x$getInverse()
  if(!is.null(j)){
  z <- x$getInverse()
  if(!is.null(z)){
    message("getting cached data")
    return(j)
    return(z)
  }
  mat <- x$get()
  j <- solve(mat,...)
  x$setInverse(j)
  j
  z <- solve(mat,...)
  x$setInverse(z)
  z
}
