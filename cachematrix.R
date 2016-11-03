## The two functions "makeCacheMatrix" and "chacheSolve" are able to compute the invers of a matrix 
## and set it into a chache.


## Chreats a special matrix by 4 functions within, which can cache the original matrix and its own inverse. 

makeCacheMatrix <- function(x) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) m <<- solve
  getinverse <- function() m
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}





## Either can call the already chached inverse of the original matrix from "makeCacheMatrix", or computes
## the inverse and caches it by "setinverse".

cacheSolve <- function(x, ...){
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get() 
  m <- solve(data, ...)
  x$setinverse(m)
  m
}


