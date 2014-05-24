## Matrix inversion is usually a costly computation. There is a benefit
## to caching the inverse of a matrix rather than computing it repeatedly.
## Here are a pair of functions that cache the inverse of a matrix.

## Computing the inverse of a square matrix can be done with the solve function.
## For example, if X is a square invertible matrix, then solve(X) returns its inverse.

## It is assumed that the matrix supplied is always invertible.


## makeCacheMatrix() creates a special "matrix" object that can cache its inverse.

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


## cacheSolve() computes the inverse of the special "matrix" returned by 
## makeCacheMatrix(). If the inverse has already been calculated (and the
## matrix has not changed), then the cachesolve should retrieve the inverse
## from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        
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
