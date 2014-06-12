## Solution for Assignmnet 2:
## Pair of functions to be used to calculate and cache the inverse of a matrix

## Creates a special "matrix" with a list of functions to get and set 
## the matrix itself and its inverse
makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) i <<- inverse
    getinverse <- function() i
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## Returns the inverse matrix of 'x'. If the inverse of 'x' has already been
## calculated, then return the cached value
cacheSolve <- function(x, ...) {
    i <- x$getinverse()
    if(!is.null(i)) {
        message("Getting cached data")
        return(i)
    }
    data <- x$get()
    i <- solve(data, ...)
    x$setinverse(i)
    i
}
