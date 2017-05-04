## Matrix inversion is usually a costly computation. There may be some benefit
## to caching the inverse of a matrix rather than compute it repeatedly.

## This function creates a special "matrix" object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setinv <- function(inv) m <<- inv
    getinv <- function() m
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}


## This function computes the inverse of the special "matrix" returned by
## makeCacheMatrix or returns it from the cache if it's already been calculated
cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    m <- x$getinv()
    if(!is.null(m)) {
        message("Getting cached data...")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setinv(m)
    m
}


## Some code to try it...
# X <- makeCacheMatrix(x = matrix(c(1,3,2,4), nrow = 2, ncol = 2))
# cacheSolve(X)
# cacheSolve(X)
# X <- makeCacheMatrix(x = matrix(c(3,1,7,5), nrow = 2, ncol = 2))
# cacheSolve(X)
