## efficient way for creating and storing an inverse of a matrix
## calculating the inverse only one and storing it in cache 

## Create an environment where the inverse "live" 
## the function returns a list of functions that can access and set the matrix and its inverse

makeCacheMatrix <- function(x = matrix()) {
        inverse <- NULL 
        set <- function(y) {
                x <<- y
                inverse <<- NULL
        }
        get <- function() x
        setInverse <- function(Inverse) inverse <<- Inverse
        getInverse <- function() inverse
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}

## calculating the inverse of a matrix or fetching it from cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inverse <- x$getInverse()
        if(!is.null(inverse)) {
                message("getting cached data")
                return(inverse)
        }
        data <- x$get()
        inverse <- solve(data, ...)
        x$setInverse(inverse)
        inverse
}

