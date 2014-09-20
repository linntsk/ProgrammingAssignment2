## makeCacheMatrix and cacheSolve are two functions that work together 
## to cache the inverse of a matrix.

## makeCacheMatrix intends to create a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        mat <- NULL
        set <- function(y) {
                x <<-y
                mat<<-NULL
        }
        get <- function()x
        setinverse <- function(inverse) mat <<- inverse
        getinverse <- function() mat
        list(set = set, get = get, 
             setinverse = setinverse,
             getinverse = getinverse)
}


## cacheSolve either computes the inverse of the special "matrix" returned by makeCacheMatrix.
## or retrieve the inverse from the cache if it already has been calculated.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        mat <- x$getinverse()
        if(!is.null(mat)){
                message("getting cached data")
                return(mat)
        }
        data <- x$get()
        mat <- solve(data,...)
        x$setinverse(mat)
        return(mat)
}
