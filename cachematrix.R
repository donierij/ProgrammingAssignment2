## These pair of functions calculate the inverse of a Matrix and cache the 
## inverse matrix for future use rather than compute it repeatedly 

## makeCacheMatrix function creates a S3 "matrix" object that can cache its
## inverse:

makeCacheMatrix <- function(x = matrix()) { 
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setsolve <- function(solve) inv <<- solve
        getsolve <- function() inv
        list(set = set, get = get,
             setsolve = setsolve,
             getsolve = getsolve)
}

## cacheSolve computes the inverse of a matrix using R's solve() function.
## If the inverse has already been calculated,then the cacheSolve retrieves
## the inverse from the cache created by makeCacheMatrix.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getsolve()
        if(!is.null(inv)) {
                message("getting cached inverse")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setsolve(inv)
        inv
}
