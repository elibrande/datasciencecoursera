## Matrix inversion is usually a costly computation and there may be some 
## benefit to caching the inverse of a matrix rather than compute it repeatedly 
## The following two functions work to cache the inverse of  a matrix

## function sets the value of the matrix, gets the value of the matrix
## sets the value of the inverse of the matrix, gets the value of the inverse of the matrix and the environment it's created in
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  evn <- environment()
  set <- function(y) {   
    x <<- y
    inv <<- NULL
  }       
  
  get <- function() x     
  setinverse <- function(inverse) inv <<- inverse   
  getinverse <- function() inv
  getevn<- function() environment()
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse,
       getevn = getevn) 
}

## funtion returns the inverse of the matrix
## checks if inverse has already been solved, if yes, gets results
## if not it computes inverse by setting the value in the cache
cacheSolve <- function(x, ...) {
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinverse(inv)
  inv     
}
