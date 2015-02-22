## This functions caches the inverse of a matrix and inverse an supplied matrix that is not available in the cache.
## If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.
## Pre-condition: The matrix supplied should be invertible.

## This function creates a special (squared) "matrix" object that can cache it's inverse.

makeCacheMatrix <- function(x = matrix()) {
        mInverse <-  NULL
        set <- function(y) {
                x <<- y
                mInverse <<- NULL
        }
        get <- function() x
        setInverse <- function(solve) mInverse <<- solve
        getInverse <- function() mInverse
        
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        mInverse <- x$getInverse()
        if(!is.null(mInverse)) {
                message("getting cached data!")
                return(mInverse)
        }
        data <- x$get()
        mInverse <- solve(data, ...)
        x$setInverse(mInverse)
        mInverse
}
