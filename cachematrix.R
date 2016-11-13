## this is the code for the 3d week programming assignment:
## to write a pair of functions that cache the inverse of a matrix

## creates an object able to store and retrieve the inverse
## matrix of a given one
makeCacheMatrix <- function(x = matrix()) {
    s <- NULL
    set <- function(y) {
        x <<- y
        s <<- NULL
    }
    get <- function() x
    setsolve <- function(solve) s <<- solve
    getsolve <- function() s
    list(set = set, get = get,
         setsolve = setsolve,
         getsolve = getsolve)
}

## executes the inverse matrix calculation 
## (and caches the result) just if
## the object created from makeCacheMatrix()
## hasn't a cached version inside, otherwise
## provides the cached version.
cacheSolve <- function(x, ...) {
    s <- x$getsolve()
    if(!is.null(s)) {
        message("getting cached data")
        return(s)
    }
    data <- x$get()
    s <- solve(data, ...)
    x$setsolve(s)
    s
}
