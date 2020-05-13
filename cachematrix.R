## These two functions work together to cache the inversion of a matrix in hopes of cutting down on an unnecessarily 
##intensive computational task. 

## This function caches the inversed matrix. 

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


## This function returns a cached inversed matrix or computes and returns the inverse of an uncached matrix. 

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
