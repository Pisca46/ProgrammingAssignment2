# Author    : Jan Vis
# Version   : 0.1 (20-8-14)
#
# This text provides a set of two funtions to avoid recalculation of the inverse 
# of a matrix any time it is needed. 
#
# The function makeCacheMatrix shall be used first. On invocation an environment
# for the values of both the original matrix and its inverse will be created.
# The function then returns a list of functions to access / use this environment.
# 
# The second function cacheSolve is to be used to get the inverse
# When invoked first, it calculates the inverse and puts (cache) it the 
# environment created when makeCacheMatrix was invoked.
# For subsequent calls call only the cached version is returned.

##                       makeCacheMatrix (x = matrix())
## -----------------------------------------------------------------------------
makeCacheMatrix <- function (x = matrix()) {
    # This function creates an environment for the original matrix (x) and inverse.
    # The argument x shall contain the matrix which inverse has to be cached.
    # The function returns a list of functions to access / use this environment.
    # makeCacheMatrix is modelled after the function makeVector in the assignment.
    
    # Create a name for the inverse with value NULL, i.e. not yet calculated
    xinv <- NULL
    
    # define a set function to reuse the environment created when 
    # makeCacheMatrix was invoked for a another matrix
    set <- function (y = matrix ()) {
        # copy y to x in this environment
        x <<- y
        # reset xinv in this environment 
        xinv <<- NULL
    }
    
    # define a get function to retrieve the matrix from
    # environment created when makeCacheMatrix was invoked
    get <- function ()  { x }
    
    # define a setinv function to cache the value of 
    # inv (which has to be the inverse matrix) in
    # xinv in the environment created when makeCacheMatrix was invoked
    setinv <- function (inv) { xinv <<- inv }
    
    # define a getinv to retrieve the value of xinv in the environment
    # created when makeCacheMatrix was invoked
    # Prior to the use of the setinv function, this value will be NULL
    # After the use of the setinv function the value will be as set by setinv
    getinv <- function()  { xinv }
    
    # incorporate the 4 functions with the same names in the list to be returned
    list(set = set, get = get, setinv = setinv, getinv = getinv)
}

##                          cacheSolve (x, ...)
## -----------------------------------------------------------------------------
cacheSolve <- function(x, ...) {
    # This function returns the inverse of a matrix in environment x
    # created with a previous call of makeCacheMatrix.
    # x shall contain the list of function returned by the previous invocation
    # ... may be used additional arguments for the solve function.
    # When invoked for the first time, it first calculates and caches the inverse
    # At subsequent invocations, the function only returns the cached inverse
    # cacheSolve is modelled after the function cachemean in the assignment
    
    # look for the cached value of the inverse
    xinv <- x$getinv()
    # if the inverse is not yet cached, i.e. if xinv == NULL, 
    # first calculate the inverse and cache it
    if(is.null(xinv)) {
        # get the cached matrix
        m <- x$get()
        # calculate its inverse
        xinv <- solve(m, ...)
        # cache the inverse
        x$setinv(xinv) 
    }
    ## return the cached inverse 
    xinv
}

## NOTE:
# According to the assignment the cacheSolve function has to perform the 
# inversion and to check wether or not the inversion was already cached.
# However it is much cleaner to implement this in getinv() 
# This would result in the following makeCacheMatrix2 function:
#
makeCacheMatrix2 <- function (x = matrix()) {
    xinv <- NULL
    
    set <- function (y) {
        x <<- y
        xinv <<- NULL
    }
    
    get <- function () { x }
    
    getinv <- function (...) {
        if (is.null(xinv)) {
            # for testing uncomment the following line
            # message ('Calculating ...')
            xinv <<- solve (x, ...)
        }   
        xinv
    }
    
    list(set = set, get = get, getinv = getinv)
}
# 
# Note that setinv() is omitted as it had become obsolete.
# 
# CacheSolve can be omitted too as it can be reduced to:
# 
cacheSolve2 <- function (x, ...) {
    x$getinv (...)
}
