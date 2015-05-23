## Pair of functions to cache inversed matrix 
## Usage:
##    m  <- makeCacheMatrix(myMatrix)
##    cacheSolve(m) 
##
## Returns special 'matrix' 
## params:  x - matrix  
## created object contains matrix,  inversed matrix (if calculated) and a set of functions to access
## both matrices:
## set, get  - to access original matrix
## setinversed, getinversed - to access inversed matrix
makeCacheMatrix <- function(x = matrix()) {
  inversed <- NULL
  set <- function(y) {
    x <<- y
    inversed <<- NULL
  }
  get <- function() x
  setinversed <- function(inv) inversed <<- inv
  getinversed <- function() inversed
  list(set = set, get = get,
       setinversed = setinversed,
       getinversed = getinversed)
}



## 
## Return a matrix that is the inverse of 'x'
## params:
## x - object created by makeCacheMatrix
##  ...   params of solve() function 
## calculates inversed matrix or returns it from cache
cacheSolve <- function(x, ...) {
  inversed <- x$getinversed()
  if(!is.null(inversed)) {
    message("getting cached data")
    return(inversed)
  }
  data <- x$get()
  inversed <- solve(data, ...)
  x$setinversed(inversed)
  inversed
}