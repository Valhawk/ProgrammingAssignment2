## makeCacheMatix and cacheSolve are a pair of functions that are designed to allow for the caching of a
## matrix and its inverse as well as providing an automatic means to calculate and return the inverse
## of the cached matrix


## This function creates an object that contains a list of functions allowing the cacheing of a Matrix
## and its inverse.

makeCacheMatrix <- function(x = matrix()) {

    i <- NULL
    set <- function(y) {
      x <<- y
      m <<- NULL
    }
    get <- function() x
    setinv <- function(inv) i <<- inv
    getinv <- function() i
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)

}


## cacheSolve takes an imput of an object containing the output of makeCacheMatrix and then if
## the inverse is already cached returns the cached data.  Otherwise it uses the solve function
## to determine the inverse of the cached matrix and then returns it.

cacheSolve <- function(x, ...) {
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
