## Put comments here that give an overall description of what your
## functions do

## function that creates a complex object containing a matrix and it reverse matrix.
## User should calcalate reverse matrix before usage and set up it here using 'cacheSolve' function
## Example of usage:
## x <-  diag(7)[ c(2:7,1), ]  ## create some matrix
## cm <- makeCacheMatrix(x)
## cacheSolve(cm)
## cm$getSolve()  ## return cache reverse matrix


makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setSolve <- function(ss) m <<- ss
        getSolve <- function() m
        list(set = set, get = get,
             setSolve = setSolve,
             getSolve = getSolve)
}


## Compute a reverse matrix of x that was created with 'makeCacheMatrix', store computed reverse matrix back to x.
## returns calculated reverse matrix
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getSolve()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setSolve(m)
        m
}
