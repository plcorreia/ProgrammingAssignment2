## This file contains a set of functions that support
## caching the result of matrix inversion to save computation
## cost when the inverse of a matrix is to be computed repeatedly 


## The makecacheMatrix function creates a special "matrix" object
## that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    get <- function() x
    setinv <- function(inv) i <<- inv
    getinv <- function() i
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}


## The cacheSolve function computes the inverse of the special "matrix" 
## returned by the makeCacheMatrix function.
## If the inverse was computed before (and the matrix has not changed), then a 
## new computation is not needed and the inverse is retrieved from the cache.

cacheSolve <- function(x, ...) {
    data <- x$get()
    inv <- x$getinv()
    # Test whether an inverseis cached and if the matrix has not changed
    if((!is.null(inv)) && (isTRUE(all.equal(x,data)))) {
        message("retrieved cached data")
        return(inv)
    }
    inv <-  solve(data) # compute inverse
    x$setinv(inv)       # store inverse in cache
    inv
}
