## Put comments here that give an overall description of what your
## functions do
## Function makeCacheMatrix() takes matrix as an input and
## get() displays the input 
## whereas Setinverse(), you can set the inverse of a matrix and send 
## that value to variable "m" which previously holding NULL value

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) m <<- solve
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
  
}


## Write a short comment describing this function
## this function takes the input X and outputs the inverse of the matrix
## it returns the catched data, if same matrix is called second time
## else returns the new output

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
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
