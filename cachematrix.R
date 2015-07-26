## This assignment involves writing two fuctions that cache the inverse of a matrix.
## The functions are named makeCacheMatrix and CacheSolve respectively 


## This function creates a special "matrix" object that can cache its inverse.
## The matrix supplied is always invertible

makeCacheMatrix <- function(x = matrix()) {
    matrixinv <- NULL
    setmatrixval <- function(y) {
        x <<- y
        matinv <<- NULL
    }
    getmatrixval <- function() x
    setmatrixinv <- function(inverse) matrixinv <<- inverse
    getmatrixinv <- function() matrixinv
    list(setmatrixval=setmatrixval, getmatrixval=getmatrixval, setmatrixinv=setmatrixinv, getmatrixinv=getmatrixinv)

}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
matrixinv <- x$getmatrixinv()
    if(!is.null(matrixinv)) {
        message("Retrieving Data...")
        return(matrixinv)
    }
    dat <- x$getmatrixval()
    matrixinv <- solve(dat)
    x$setmatrixinv(matrixinv)
    matrixinv
} 


