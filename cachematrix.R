## This file contains the following two functions:
##   1. makeCacheMatrix - creates a "matrix object" that can store matrix 
##      value and cache its inverse.
##   2. cacheSolve - function that returns "matrix object" inverse.


## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
    # Function arguments:
    #    x - matrix
    # Methods:
    #   set(y) - sets matrix value to y
    #   get() - returns matrix value of "matrix object"
    #   set_inv(y) - sets value of inverted matrix to y (inverted matrix 
    #   caching)
    #   get_inv() - returns value of inverted matrix
    # Function returns:
    #   list of all "matrix object" methods
    
    x_inv <- NULL  # Clear cache while creating new matrix object

    set <- function(y) {
        x <<- y
        x_inv <<- NULL
    }
    get <- function() x
    set_inv <- function(y)  x_inv <<- y
    get_inv <- function() x_inv
    list (set = set, get = get, set_inv = set_inv, get_inv = get_inv)    
}


## The following function returns a matrix that is the inverse of "matrix
## object" x. The function checks if the inverted matrix has been cached and
## if so, returns the value from cache. Otherwise, if the cache is empty, 
## computes the inverted matrix value, sets the cache to that value and also 
## returns that computed value.

cacheSolve <- function(x, ...) {
        
    x_inv <- x$get_inv()
    if(!is.null(x_inv)) {
        message("getting cached data")
                return(x_inv)
    }
        data <- x$get()
        x_inv <- solve(data, ...)
        x$set_inv(x_inv)
        x_inv    
}
