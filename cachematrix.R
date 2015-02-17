## The function makeCacheMatrix creates a "matrix" object that can cache its
## inverse (more specifically, it creates a list containing a function to set/get
## the value of the matrix and set/get the value of inversed matrix).

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y){
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setmatrix <- function(solve) m <<- solve
  getmatrix <- function() m
  list(set=set, get=get, setmatrix=setmatrix, getmatrix=getmatrix)
}


## The function cacheSolve calculates the inversed matrix of a matrix created
## using makeCacheMatrix function, yet it checks whether the inversed matrix
## has already been calculated - if so, it skips computation and gets this matrix
## from the cache; if not - this function calculates it.

cacheSolve <- function(x, ...) {
  m <- x$getmatrix()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setmatrix(m)
  m
        ## Return a matrix that is the inverse of 'x'
}
