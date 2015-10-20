## This pair of functions (makeCacheMatrix and cacheSolve) allows to cache
## the inverse matrix of an invertible matrix to avoid unnecessary repeated
## computations.

## The function 'makeCacheMatrix' creates a special kind of matrix object
## that can cache the matrix itself (x) as well as its inverse matrix (i).
## The matrix itself is stored and recalled by set() and get().
## The inverse matrix is stored and recalled by setinverse() and getinverse().

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) i <<- inverse
        getinverse <- function() i
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## The function cacheSolve uses the special kind of matrix from makeCacheMatrix.
## It first checks whether a value for the inverse matrix is available. If it is
## available it will return that cached value. Otherwise it will calculate the inverse
## matrix from the invertible matrix, cache it in the special matrix object and return
## inverse matrix.

cacheSolve <- function(x, ...) {
        i <- x$getinverse()
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        data <- x$get()
        i <- solve(data, ...)
        x$setinverse(i)
        i
}
