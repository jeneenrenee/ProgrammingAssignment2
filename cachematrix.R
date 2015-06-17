## cachematrix.R consists of two functions.  makeCacheMatrix takes a
## square matrix as input and caches its inverstion.  cacheSolve takes
## the output of makeCacheMatrix and returns the either the cached inversion,
## or computes the inversion if it hasn't been cached.

## Cache the inverse of a matrix.

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setinv <- function(solve) m <<- solve
    getinv <- function() m
    list(set=set, get=get, setinv=setinv, getinv=getinv)
}



## Return the cached inverted matrix, or calculate and return the inverse if
## it isn't in memory.

cacheSolve <- function(x, ...) {
    m <- x$getinv()
    if (!is.null(m)) {
        message ('getting cached data')
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setinv(m)
    message ('getting uncached data')
    m
}
