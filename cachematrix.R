## Put comments here that give an overall description of what your
## functions do

##  creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  cache_inv_x<-NULL
  #Set function
  set <- function(y) {
    x <<- y
    cache_inv_x <<- NULL
  }
  #Get function
  get <- function() x
  #solve() is used to calculate inverse
  setinverse <- function(solve) cache_inv_x <<- solve
  getinverse <- function() cache_inv_x
  list(set = set, get = get, setinverse = setinverse,getinverse = getinverse)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), then 
## the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  cache_inv_x <- x$getinverse()
  if(!is.null(cache_inv_x)) {
    message("getting cached data")
    return(cache_inv_x)
  }
  data <- x$get()
  cache_inv_x <- solve(data, ...)
  x$setinverse(cache_inv_x)
  cache_inv_x
}

