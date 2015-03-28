## These functions allow to create a List of functions that sets (caches) and retrieve a matrix and its inverse
## If the matrix is set a new value, the catched Inverse is deleted and need to be calculated again

## Similar to the example, this function creates a list of functions in its environment to set and get the value of the matrix
## and its inverse in this environment

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        # This ensures that previous matrix inverse calculations are erased then the matrix is modified
        inv <<- NULL
    }
    get <- function() x
    setInverse <- function(Minv) inv <<- Minv
    getInverse <- function() inv
    list(set=set, get=get, setInverse=setInverse, getInverse=getInverse)
}


## This function returns the value of the cached matrix inverse
## if it has not been calculated before, then the inverse calculation is done and cached before returning it.
## if the inverse can not be calculated for different reasons, a message is displayed and a NULL returned 

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inv <- x$getInverse()
    if (!is.null(inv)) {
        return(inv)
    }
    matrix <- x$get()
    detM <- det(matrix)
    # check for NA 
    if (is.na(detM)) {
        message("Inverse can not be calculated, matrix contains NA element(s)")
        return(inv)
    }
    # Check for matrix determinant to be different than zero
    if (detM == 0) {
        message("Inverse can not be calculated, matrix determinat is 0")
    } else {
    inv <- solve(matrix, ...)
    x$setInverse(inv)
    }
    inv
}
