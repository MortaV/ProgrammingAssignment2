## makeCacheMatrix() and cacheSolve() is helpful when you want to cache inverse 
## of a matrix. Inverse matrix computation takes a lot of time and resources so 
## sometimes it is useful to "reuse" already calculated value.
## Example of use:
## mat <- matrix(c(1,2,3,4), nrow = 2)
## my_cache <- makeCacheMatrix(mat)
## cacheSolve(my_cache)
## cacheSolve(my_cache)


## creates an object which can be used in a cacheSolve() function later on

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) m <<- inverse
    getinverse <- function() m
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## populates or/and retrieves the inverse from an object of type makeCacheMatrix()

cacheSolve <- function(x, ...) {
    m <- x$getinverse()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setinverse(m)
    m
}