## This function stores the inverse of a matrix

makeCacheMatrix <- function(x = matrix()) {
    inv = NULL
    
    ## if a new matrix is given, override x
    set = function(y) {
        x <<- y
        inv <<- NULL
    }
    
    get = function() x
    setinv = function(inverse) inv <<- inverse 
    getinv = function() inv
    list(set=set, get=get, setinv=setinv, getinv=getinv)
}


## Inverting a matrix can be a expensive computation
## Calculate the inverse and store it in the object

cacheSolve <- function(x, ...) {
    ## Get the inverse if any
    inv = x$getinv()
    
    # if the object has already calculate the inverse, return it
    if (!is.null(inv)){
        # get it from the cache and skips the computation. 
        message("getting cached data")
        return(inv)
    }
    
    # otherwise get the matrix from the object and calculate it!
    inv = solve(x$get(), ...)
    
    # now store the value in the object for further look up
    x$setinv(inv)
    
    return(inv)
}