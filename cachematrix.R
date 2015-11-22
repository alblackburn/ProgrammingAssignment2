## The makeCacheMatrix function will return a list including four variables.
## These four variables will allow you to set or view the matrix, and also to
## set or view the inverse of the matrix.

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        
        get <- function() x
        
        setinv <- function(inverse) inv <<- inverse
        
        getinv <- function() inv
        
        list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## The cacheSolve function will first check to see if the inverse has be set (cached),
## and if so, it will return this cached value instead of recalculating it.
## If it is not cached, it will calculate the inverse and then cache it.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinv()
        
        if(!is.null(inv)) {
                message("Getting cached data...")
                return(inv)
        }
        
        data <- x$get()
        
        inv <- solve(data, ...)
        
        x$setinv(inv)
        
        inv
}
