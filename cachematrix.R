## The functions below are used to create a matrix, compute its
## inverse and store its value in the cache to speed up the computations.

## This function creates a special matrix, which consist of a list
## of functions to retrieve or set the matrix itself and its cached inverse.

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinv <- function(inverse) inv <<- inverse
        getinv <- function() inv
        list(set = set, get = get,
			setinv = setinv,
            getinv = getinv)

}


## This function takes the special matrix defined above as an input and
## computes its inverse, but only if is not altredy present in the cache.
## The first if clause checks this condition and since the "set" function
## defined above sets the inverse to NULL when the matrix is changed, the
## inverse is actually computed only if not already present in the cache and
## if the matrix hasn't been modified.

cacheSolve <- function(x, ...) {
        inv <- x$getinv()
        if(!is.null(inv)) {
                message("Getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setinv(inv)
        inv
}
