## MakeCacheMatrix create a special matrix that is used to cache the results of the inverse matrix
##it has 4 functions set, get, setinverse and getinverse. Set and get allow you to set and get the value of the matrix
##setinverse and getinverse allow you to set the inverse matrix and get the value of the inverse matrix
##these functions are used in the cacheSolve function to get the cached data
makeCacheMatrix<-function(x=matrix()){
  m=NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(mean) m <<- mean
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}
##this is the function that calculates the inverse of the matrix using the solve function, iff the inverse does not already exist in the cache
##it uses the functions in the makeCacheMatrix to get the cached data or set the inverse
cacheSolve <- function(x, ...) {
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