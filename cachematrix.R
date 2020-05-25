##     makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
##cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
##If the inverse has already been calculated (and the matrix has not changed), 
##then the cachesolve should retrieve the inverse from the cache.

## Caching the inverse of a matrix!

makeCacheMatrix <- function(x = matrix()) {
       
       
                m <- NULL
                set <- function(y) {
                        x <<- y
                        m <<- NULL
                }
                get <- function() x
                setinv <- function(solve) m <<- solve
                getinv <- function() m
                list(set = set, get = get,
                     setinv = setinv,
                     getinv = getinv)
        
         
        
}


## Calculate Inverse of a matrix

cacheSolve <- function(x, ...) {
       
        cacheinv <- function(x, ...) {
                m <- x$getinv()
                if(!is.null(m)) {
                        message("getting cached data")
                        return(m)
                }
                data <- x$get()
                m <- solve(data, ...)
                x$setinv(m)
                m
        }
}
