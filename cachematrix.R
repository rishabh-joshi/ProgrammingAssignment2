# COURSERA DATA SCIENCE SPECIALIZATION - R PROGRAMMING
# PEER ASSESSMENT

# Below are two functions that are used to create a special object 
# that stores a matrix and caches its inverse.

## The first function, makeCacheMatrix creates a special "matrix" object,
## which is really a list containing functions to do the following:
# 1. set the value of the matrix
# 2. get the value of the matrix
# 3. set the value of the inverse
# 4. get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    set_inv <- function(inverse) inv <<- inverse
    get_inv <- function() inv
    list(set = set, get = get,
         set_inv = set_inv,
         get_inv = get_inv)
}


# The following function calculates the inverse of the special "matrix" 
# created with the above function. However, it first checks to see 
# if the inverse has already been calculated. If so, it gets the 
# inverse from the cache and skips the computation. Otherwise, it 
# calculates the inverse of the matrix and sets the value of the 
# inverse in the cache via the set_inv function.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inv <- x$get_inv()
    if(!is.null(inv)) {
        message("getting cached inverse")
        return(inv)
    }
    mat <- x$get()
    inv <- solve(mat, ...)
    x$set_inv(inv)
    inv
}
