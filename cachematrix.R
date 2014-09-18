## A pair of functions to cache the inverse of a matrix

## This function creates a matrix object that can cache
## the matrix's inverse

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


## Computes the inverse of the matrix from makeCacheMatrix
## if inverse is already calculated and not changed,
## cachSolve retrieves the inverse from the cache

cacheSolve <- function(x, ...) {
    i <-x$getinverse()
    if(!is.null(i)) {
        message("getting cached data")
        return(i)
    }
    data <- x$get()
    i <- solve(data, ...)
    x$setinverse(i)
    i
}
