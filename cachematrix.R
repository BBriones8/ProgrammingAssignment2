## The following functions will cache the inverse of a matrix.


## This function makes a matrix that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinv <- function(inverse) inv <<- inverse
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}


## This function will inverse the matrix returned by the function above. 
## This function will retrieve the inverse from the cache if the inverse has already been calculated.

cacheSolve <- function(x, ...) {
    inv <- x$getinverse()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setinv(inv)
    inv
}
