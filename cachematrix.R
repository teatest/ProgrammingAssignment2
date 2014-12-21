## The function that creates a special "matrix" object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setinvert <- function(invert) m <<- invert
    getinvert <- function() m
    list(set = set, get = get,
         setinvert = setinvert,
         getinvert = getinvert)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), then cacheSolve retrieves the inverse from the cache.
cacheSolve <- function(x, ...) {
    m <- x$getinvert()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setinvert(m)
    m
}
