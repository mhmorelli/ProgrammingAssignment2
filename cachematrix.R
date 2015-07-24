## These two functions are used to create a special object that stores a square
## matrix and caches its inverse.

## makeCacheMatrix create a special matrix

makeCacheMatrix <- function(x = matrix()) {
    inv.x <- NULL
    set <- function(y) {
        x <<- y
        inv.x <<- NULL
    }
    get <- function() x
    setInverse <- function(inverse) inv.x <<- inverse
    getInverse <- function() inv.x
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
    
}


## cacheSolve calculates and caches a inverse matrix of the special matrix 
## created with the above function

cacheSolve <- function(x, ...) {
    inv.x <- x$getInverse()
    if(!is.null(inv.x)) {
        message("getting cached data")
        return(inv.x)
    }
    data <- x$get()
    inv.x <- solve(data, ...)
    x$setInverse(inv.x)
    inv.x  ## Return a matrix that is the inverse of 'x'
}
