## Put comments here that give an overall description of what your
## functions do

The makeCacheMatrix and cacheSolve functions work as a pair to provide
a cached matrix invert (solve).  The inverted matrix can be accessed
multiple times, but it only computed once.

Example of a non-cached matrix invert:

> amat <- matrix(c(3,1,2,1),nrow=2,ncol=2)
> solve(amat)
     [,1] [,2]
[1,]    1   -2
[2,]   -1    3

Example of a cached matrix invert:

> amat <- matrix(c(3,1,2,1),nrow=2,ncol=2)
> amatc = makeCacheMatrix(amat)
> yc=cacheSolve(amatc)
     [,1] [,2]
[1,]    1   -2
[2,]   -1    3


## This function contains the matrix to be cached, along with the
## cached value of any inverse obtained.

makeCacheMatrix <- function(x = matrix()) {
#
# Initial value is NULL
#
        m <- NULL 
#
# Set the "x" (calculated) value and clear the "m" cache value
#
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
#
# Get the "x" (calculated) value
#
        get <- function() x
#
# Set the m (cached) value
#
        setsolve <- function(solve) m <<- solve
#
# Get the m (cached) value
#
        getsolve <- function() m
#
# Return the list of methods
#
        list(set = set, get = get,
             setsolve = setsolve,
             getsolve = getsolve)
}


## This function returns the inverse of the matix passed, either by
## calculating the inverse directly, or by returning the cached
## inverse from a prior calculation.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
#
# First, try the cached value ... if it's set, use it.
#
        m <- x$getsolve()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
#
# Not cached -- solve (invert) the input matrix and save it in
# the cache.
#
        data <- x$get()
        m <- solve(data, ...)
        x$setsolve(m)
        m
}
