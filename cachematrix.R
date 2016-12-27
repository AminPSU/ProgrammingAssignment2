## These two functions can be used to calculate inverse of a inversable matrix and store its result for
## future calls, as a cashed value. Calculation just happen at first call and then, while the input matrix does
## not change, the output will be retrieved from cache.

## makeCacheMatrix, takes matrix x as its input and the result will be a list of 4 function.
## This function works exactly the same way as the example code, makeVector.

makeCacheMatrix <- function(x = matrix()) {
inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(inverse) inv <<- inverse
  getinv <- function() inv
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## cacheSolve function takes an makeCacheMatrix object as its input. At it first run, it will calculate the inverse
## of that matrix, and both return it as result and store it in cache for future calls. From the second run, this function
## will give us the message "getting cached data" and then the result will be printed.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
inv <- x$getinv()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinv(inv)
  inv
}
