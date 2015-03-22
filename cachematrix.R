## These functions allow to create a List of functions that allows to set a value for a matrix
## when the inverse of the matrix has been calculated it is cached for further use
## If the matrix is set a new value, the catched Inverse is deleted and need to be calculated again

## Similar to the example, this function creates a vector of functions to set and get the value of the matrix
## in this environment, also allows to set and get the Inverse of the matrix

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
## if it has not been calculated before, then the inverse calculation is done and cached. The inverse is returned
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
