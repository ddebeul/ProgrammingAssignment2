## Matrix inversion is usually a costly computation and their may be some benefit to caching the inverse of a matrix rather than compute it repeatedly (there are also alternatives to matrix inversion that we will not discuss here). Your assignment is to write a pair of functions that cache the inverse of a matrix.
## Write the following functions:
## makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
## cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.
## Computing the inverse of a square matrix can be done with the solve function in R. For example, if X is a square invertible matrix, then solve(X) returns its inverse.


# makeCacheMatrix: return a list of functions to:
# 1. Set the value of the matrix
# 2. Get the value of the matrix
# 3. Set the value of the inverse
# 4. Get the value of the inverse
makeCacheMatrix <- function(x = matrix()) {
        inv  <- NULL
        set  <- function(y){
                x <<- y
                inv <<- NULL 
        }
        get  <- function() x
        setinverse  <- function(inverse) inv  <<- inverse
        getinverse  <- function() inv
        list(set= set, get = get, 
             setinverse = setinverse, 
             getinverse = getinverse)
}


# cacheSolve: Compute the inverse of the matrix. If the inverse is already
# calculated before, it returns the cached inverse.
cacheSolve <- function(x, ...) {
        inv  <- x$getinverse()
        if (!is.null(inv)){
                message("getting cached data")
                return(inv)
        }
        data  <- x$get()
        inv  <- solve(data, ...)
        x$setinverse(inv)
        inv
}