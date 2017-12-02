#The makeCacheMatrix-cacheSolve tandem essentially makes the process of matrix inversion a less costly computation by caching the inverse of a matrix.

#makeCacheMatrix creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(m = matrix()) {
        i <- NULL
        set <- function(x) {
                m <<- x
                i <<- NULL
        }
        get <- function() m
        setInverse <- function(solve) i <<- solve
        getInverse <- function() i
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}

#cacheSolve computes the inverse of the special "matrix" returned by makeCacheMatrix. In cases where the inverse has already been calculated (and the matrix has not changed), cacheSolve retrieves the inverse from the cache.
cacheSolve <- function(m, ...) {
        i <- m$getInverse()
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        data <- m$get()
        i <- solve(data, ...)
        m$setInverse(i)
        i
}
