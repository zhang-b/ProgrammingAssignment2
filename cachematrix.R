## The two functions solve and cache the inverse of a matrix
## to avoid potential repeating inversion of the same matrix

## The function creates a list of four functions
## to set and get a matrix x and its inverse x_inv
## with NULL value indicating a matrix not cached.

makeCacheMatrix <- function(x = matrix()) {
        x_inv <- NULL
        set <- function(y){
                x <<- y
                x_inv <<- NULL
        }
        get <- function() x
        setinv <- function(y) x_inv <<- y
        getinv <- function() x_inv
        list(set = set, get = get,
             setinv = setinv, getinv = getinv)
}


## The function gets the cached inverse, when there is one,
## of a matrix x; otherwise, it solves for the inverse.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        x_inv <- x$getinv()
        if(!is.null(x_inv)){
                message("getting cached data")
                return(x_inv)
        }
        data <- x$get()
        x_inv <- solve(data)
        x$setinv(x_inv)
        x_inv
}
