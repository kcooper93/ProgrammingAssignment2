## This is a function which creates a matrix it then follows 4 steps
## set the value of the matrix
## get the value of the matrix
## set the value of the inverse
## get the value of the inverse
## The matrix can then cache its inverse. 

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) inv <<- inverse
        getinverse <- function() inverse
        list(set = set, get = get,
             setinverse = set inverse,
             getinverse = get inverse)
}


## Using makeCacheMatrix the function computes the inverse returned. 
## If the inverse has already been caluclated and the matrix hasn't changed
## cacheSolve will retrieve the matrix from the cache.
## It is assumed the matrix is always invertible.

cacheSolve <- function(x, ...) {
      
        inv <- x$getinverse()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- inverse(data, ...)
        x$setinverse(inv)
        inv
}
