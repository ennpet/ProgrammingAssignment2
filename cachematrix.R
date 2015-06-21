## caching the inverse of a matrix to avoid 
## a repeated and (usually) costly computation.

## creates a  "matrix" object that can cache its inverse and
## has accessor methods to set and retrieve the cached inversed matrix.

makeCacheMatrix <- function(x = matrix()) {
    
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    
    get <- function() x
    setSolve <- function(solve) m <<- solve
    getSolve <- function() m
    
    list(set = set, get = get,
         setSolve = setSolve,
         getSolve = getSolve)
    
}


## calculates the inverse of a matrix created with the "makeCacheMatrix" method. 
## It first checks to see if the inverse has already been calculated. 
## If so, it gets the inverse matrix from the cache. Otherweise calculates and sets the inverse
## in the cache via the setSolve function.

cacheSolve <- function(x, ...) {
    
    m <- x$getSolve()
    
    if(!is.null(m)) {
        return(m)
    }
    
    data <- x$get()
    m <- solve(data, ...)
    x$setSolve(m)
    m
}
