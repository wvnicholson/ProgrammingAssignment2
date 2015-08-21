## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

# makeCacheMatrix creates a special matrix object, which is really
# a list containing a function to:
# 1. set the value of the matrix
# 2. get the value of the matrix
# 3. set the value of the matrix's inverse
# 4. get the value of the matrix's inverse

makeCacheMatrix <- function(x = matrix()) {
  xInv <- NULL
  set <- function(y){
    x <<- y
    xInv <<- NULL
  }
  get <- function() x
  setinv <- function(solve) xInv <<- solve
  getinv <- function() xInv
  list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## Write a short comment describing this function
# cacheSolve computes the inverse of the special matrix object
# returned by makeCacheMatrix above.  If the inverse has
# already been calculated (and the matrix has not
# changed), then cacheSolve retrieves the inverse
# from the cache, using get.  Otherwise, it
# computes the inverse using solve and sets the
# value of the inverse in the cache using setinv.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  xinv <- x$getinv()
  if(!is.null(xinv)){
    message("getting cached data")
    return(xinv)
  }
  data <- x$get()
  xinv <- solve(data, ...)
  x$setinv(xinv)
  xinv
}
