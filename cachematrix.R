## These are the pair of functions that cache the inverse of matrix

##  This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
    inverse <- NULL
    set <- function(y){
        x <<- y
        inverse <<- NULL
    }
    get <- function() x
    setInvese <- function(inv) inverse <<- inv
    getInverse <- function() inverse
    list(set = set, get = get, setInvese = setInvese, getInverse = getInverse)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
#If the inverse has already been calculated (and the matrix has not changed), 
#then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    inverse <- x$getInverse
    if(!is.na(inverse)){
        message("getting cached data")
        inverse
    }
    data <- x$get()
    inverse <- solve(data, ...)
    inverse
    }
