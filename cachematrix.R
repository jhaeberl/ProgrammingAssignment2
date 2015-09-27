## A pair of functions, makeCacheMatrix and cacheSolve, that implement
## an enhanced matrix type that can cache its inverse.

## makeCacheMatrix creates a special matrix that is capable of
## storing its inverse. The motivation is that the inverse is
## computed once but may be used as often as required without
## incurring the cost of re-computing it every time.
## The special matrix type constructed by makeCacheMatrix exposes
## four methods to manipulate the matrix and its inverse:
##    set : sets the underlying (ordinary) matrix to a specific value
##    get : returns the underlying matrix
##    setinv : sets the inverse of the matrix to the specified value
##    getinv : returns the inverse of the underlying matrix

makeCacheMatrix <- function(x = matrix()) {
    xinv = NULL
    set <- function(y) {
        x <<- y
        xinv <<- NULL
    }
    get <- function() x
    setinv <- function(inv) xinv <<- inv
    getinv <- function() xinv
    list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## cacheSolve returns the inverse of a special matrix created by
## makeCacheMatrix. It only computes the inverse if it is not
## already cached and caches it. Otherwise it simply returns the
## cached value.
## Note: cacheSolve assumes that the specified matrix actually is
## invertible!

cacheSolve <- function(x, ...) {
    ## Returns a matrix that is the inverse of 'x'
    matinv <- x$getinv()
    if (!is.null(matinv)) {
      message("getting cached data")
      return(matinv)
    }
    mat = x$get()
    matinv = solve(mat, ...)
    x$setinv(matinv)
    matinv
}
