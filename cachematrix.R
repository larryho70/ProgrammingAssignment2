## Contains two functions:
##      makeCacheMatrix
##      cacheSolve
## which implements a method to cache the result of the solve R function against a matrix so that 
## succeeding calls will return the saved result instead of calling the solve function again.
## (NOTE: The solve function returns the inverse of a matrix)

## makeCacheMatrix creates a special Matrix which is really a list containing functions that implements
## the ff operations:
##      1) a set function to create the matrix and its inverse which is initially set to NULL.
##              the matrix is assumed to be a valid NxN matrix
##      2) a get function to return the contents of the matrix 
##      3) a setinv function which saves the value of the inverse matrix 
##      4) a getinv function which returns the saved inverse matrix

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinv <- function(inverse) inv <<- inverse
        getinv <- function() inv
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}


## cacheSolve is a version of solve R function that returns the inverse of a matrix.

## It is different from the regular solve in that it saves the result into cache and
## succeeding calls to cacheSolve will return the cached result instead of re-"solving"
## the inverse.  In this way, cacheSolve runs faster since it doesn't have to calculate
## over and over the inverse of the same matrix.  

## When cacheSolve detects a cached solution, it sends a message notifying the user that
## the solution displayed came from the cache.

## cacheSolve requires a matrix that has been initialized by the makeCacheMatrix 
## function above as a parameter.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinv()
        if(!is.null(inv)) {
                message("getting cached inverse")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setinv(inv)
        inv
}
