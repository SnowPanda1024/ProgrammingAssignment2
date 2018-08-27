## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## Takes a matrix and returns a vector that can be passed to cacheSolve
## Caches the inverse and invalidates the cache when a matrix is set
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(i) inv <<- i
  getinverse <- function() inv
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Write a short comment describing this function
## Takes the vector from the previous function and returns the inverse of the matrix
## If the inverse of the matrix is already in the cache then uses the cache, otherwise calculates and adds to the cache
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  X <- x$get()
  inv <- solve(X)
  x$setinverse(inv)
  inv       
}
