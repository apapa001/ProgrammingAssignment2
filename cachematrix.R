## The following functions can be used in order to retrieve (or obtain, 
## if it has not been calculated before) the inverse of any invertible matrix. This will save
## time, as the already calculated inverses will be cached and, thus, retrievable.


## The first function, makeCacheMatrix creates a special "vector", 
## i.e. a list containing four different functions.

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setInverse <- function(Inverse) inv <<- Inverse
        getInverse <- function() inv
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}



## The following function checks to see if the inverse of the vector created above
## has already been computed. If so, it retrieves it from the cache without computing it.
## If not, it calculates it.

cacheSolve <- function(x, ...) {
        inv <- x$getInverse()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setInverse(inv)
        inv
}