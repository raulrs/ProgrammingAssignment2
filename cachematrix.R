## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## 'x' would be input matrix and 'm' would be the inverted of the input matrix
## The function below will calculate the inverse of a matrix and store it in a cache.

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) m <<- solve
        getinverse <- function() m
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)

}


## Write a short comment describing this function
## This function calculate the inverse of the input matrix, only if this matrix was not calculated before to obtain its inversed.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinverse()
        
        ## 'if' function check if already is calculated in the cache the inverted matrix and return 'm' if it was indeed calculated.
        
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        
        ## Otherwise, search on parents environments and look for the matrix which has not been inverted yet and get the invert with the code below.
        
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m
}
