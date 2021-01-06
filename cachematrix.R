
## I  set the input x as a matrix
## and then set the solved value "inv" as a null

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setsolve <- function(solve) inv <<- solve
    getsolve <- function() inv
    list(set = set, get = get,
         setsolve = setsolve,
         getsolve = getsolve)
}
##
## Same here, changed "mean" to "solve" and "m" to "inv"
cacheSolve <- function(x, ...) {
    inv <- x$getsolve()
    if(!is.null(inv)) {
        message("getting inversed matrix")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setsolve(inv)
    inv
}