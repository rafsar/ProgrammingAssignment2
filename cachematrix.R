## These functions calculate the inverse of a matrix
## Once it is already calculated, it caches the solved answer

## Set and get a cached matrix

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setInvMtx <- function(cacheSolve) m <<- cacheSolve
        getInvMtx <- function() m
        list(set = set, get = get, setInvMtx = setInvMtx, getInvMtx = getInvMtx)
}


## Solve the inverse of a matrix created by makeCacheMatrix

cacheSolve <- function(x, ...) {
        m <- x$getInvMtx()
        if(!is.null(m)) {
                message("Getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setInvMtx(m)
        m
}
