## Script to cache or calculate inverse of matrix

## Function to create list containing function to
## Set and get value of matrix
## Set and get value of inverse
## This list will be used as the input to cacheSolve

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL ##<<- operator sets value in new environment
  }
    get <- function() x
    setInverse <- function(mean) inv <<- inverse
    getInverse <- function() inv
    list(set = set, get = get,
        setInverse = setInverse,
        getInverse = getInverse)
}


## Function to check if inverse is in cache and retrive directly
## If not, calculates inverse and sets value in cache

cacheSolve <- function(x, ...) {
    inv <- x$Inverse()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setInverse(inv)
    inv
}