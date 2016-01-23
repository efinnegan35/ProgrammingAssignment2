## Week 3 programming assignment for R programming with Dr. Peng
## 

## The makeCacheMatrix function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL                         # i is a placeholder for a future value
  set <- function(y) {              # defines a function to set the matrix x = to a new matrix y
    x <<- y                         # 
    i <<- NULL                      # resets i
}
  get <- function() x               #returns the vector x
  setinv <- function(solve) i <<- solve   #sets the inverse, i, to solve
  getinv <- function() i         # get the value of the inverse
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}

## The cacheSolve function computes the inverse of the special "matrix" returned by makeCacheMatrix 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  i <- x$getinv()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinv(i)
  i
}
