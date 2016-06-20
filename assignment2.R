##makeCacheMatrix produces a special matrix object to cache 
##its inverse

makeCacheMatrix <- function(x = matrix()){
  ##Inverse property
  m <- NULL

##Set matrix  
set <- function(y){
  x <<- y
  m <<- NULL
}
##Get matrix
get <- function() {
  x
}
##Set inverse of matrix
setInverse <- function(inverse) {
  i <<- inverse
}
## Get inverse of matrix
getInverse <- function() {
  m
}
##Return list of methods
list (set = set, get = get,
      setInverse = setInverse,
      getInverse = getInverse)
}

##cacheSolve computes inverse of matrix returned by makeCacheMatrix
##This function should retrieve inverse from cache if inverse was already 
##calculated and matrix did not change
cacheSolve <- function(x,...) {
  ##Return inverse of x matrix
  m <- x$getInverse()
  ##Return matrix
  if( !is.null(m) ) {
    message("getting cached data")
    return(m)
  }
  ##Get matrix from object
  data <- x$get()
  ##Calculate inverse via multiplication
  m <- solve(data) %*% data
  ##Set inverse to object
  x$setInverse(m)
  ##Return matrix
  m
}