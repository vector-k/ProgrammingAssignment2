## Matrix list object w/cached inverse
##
## These functions allow a user to create a matrix-backed list object
## and interact with it via a list of functions; the object can cache
## the inverse of the matrix, allowing convenient access to the matrix
## solution without looking it up.
## 
##
## Example: compare with solve(h8)
## > hilbert <- function(n) { i <- 1:n; 1 / outer(i - 1, i, "+") }
## > h8 <- hilbert(8)
## > ch8 <- makeCacheMatrix(h8)
## > sch8 <- cacheSolve(ch8)
## > sch8 <- cacheSolve(ch8)

## Create a matrix-backed list object from a matrix
makeCacheMatrix <- function(x = matrix()) {
    ## object to store inverse
    matInv <- NULL
    
    ## getter/setter for matrix
    get <- function() x
    set <- function(y) {
        x <<- y
        ## matrix set, clear inverse
        matInv <<- NULL  
    }
    
    ## getter/setter for inverse of matrix
    getInverse <- function() matInv
    setInverse <- function(solve) matInv <<- solve
    
    ## add to a list for convenient access
    list(get = get,
         set = set,
         getInverse = getInverse,
         setInverse = setInverse)
}


## Solve the matrix inside a matrix-backed list object and cache the sol'n
cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    matInv <- x$getInverse()
    if(!is.null(matInv)) {
        message("Getting cached matrix inverse")
        return(matInv)
    }
    mat <- x$get()
    matInv <- solve(mat, ...)
    x$setInverse(matInv)
    matInv
}
