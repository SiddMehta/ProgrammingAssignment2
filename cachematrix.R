
## Matrix inversion is usually a costly computation and there may be some benefit to caching the inverse of a matrix rather than compute it repeatedly. The
## following two functions are used to cache the inverse of a matrix :


## makeCacheMatrix creates a list containing labelled functions, so as to
## a. set value of the matrix
## b. get value of the matrix
## c. set value of inverse of the matrix
## d. get value of inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) inv <<- inverse
    getinverse <- function() inv
    list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


##   Function below returns the inverse of inputted matrix. It will check if
##   the inverse has already been computed. If it has, then it gets the result and skips that particular iteration. 
##   If not, it calcualtes and prints the inverse, and then sets the value in the cache through
##   the function "setinverse".

## This function always assumes an invertible matrix.
cacheSolve <- function(x, ...) {
    inv <- x$getinverse()
    if(!is.null(inv)) {
        message("getting cached files!")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data)
    x$setinverse(inv)
    inv
}
