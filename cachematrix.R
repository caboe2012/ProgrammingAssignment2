## These functions  create a "matrix" object, then calculate the inverse of this matrix object, and
## finally after computing this inverse one time, they cache the inverse so that it no longer has to be computed
## when needed in the future.

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        getmatrix <- function() x
        cacheinverse <- function(inverse) m <<-inverse
        getinverse <- function() m
        list(getmatrix=getmatrix, cacheinverse=cacheinverse, getinverse=getinverse)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinverse()
        if(!is.null(m)) {
                message("looks like this inverse has already been computed, getting that cached data now")
                return(m)
        }
        message("looks like this is a new matrix, computing the inverse now...")
        dat <- x$getmatrix()
        m <- solve(dat)
        x$cacheinverse(m)
        m
}
