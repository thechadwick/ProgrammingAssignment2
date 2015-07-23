## These functions provide the ability to calculations the inverse of a matrix
## They province caching for previously calculated output

## Creates an object which wraps a matrix so a 
## calculated inverse can be cached
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


## Returns an inverse of a matrix
cacheSolve <- function(x, ...) {
    ## If we have a cached value return it
    i <- x$getinverse()
    if (!is.null(i)) {
        message("getting cached data")
        return(i)
    }
    
    ## Otherwise calculate the inverse and store it
    data <- x$get()
    i <- solve(data)
    x$setinverse(i)
    i
}
