## In this code we define two functions to solve potentially the inverse of 
## a matrix. If it's an operation you've done recently, we 
## will cache it first, and solve the inverse using the cache. 
## If it's not cached, we solve the inverse directly

## The first function makeCacheMatrix makes the initial definition 
## of the function we're gonna use to solve de inverse and establishes 
## the data (the matrix)

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setsolve <- function(solve) inv <<- solve
  getsolve <- function() inv
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
}

## The second funtion cacheSolve checks first if we already 
## have calculated the inverse of the matrix and it's cached. If so, we do the inverse
## using the cached data. If not, we solve de inverse of the matrix(data input).

cacheSolve <- function(x, ...) {
  inv <- x$getsolve()
  if(!is.null(inv)) {
    message("Getting cached data to calculate the inverse matrix")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setsolve(inv)
  inv
}

