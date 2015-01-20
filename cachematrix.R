## Adam Smith
## 1.19.2014

## Put comments here that give an overall description of what your
## functions do
## This script creates two functions that will cache the inverse of a matrix
## The first (makeCachesMatrix) creates a list of functions that the second will call
## The second (cacheSolve) checks to see if a new matrix is being passed to it.
## If it is new then it calculates a new inverse and stores it.
## If it is the same as the old matrix, then it sends the cached inverse

## Write a short comment describing this function
## This function creates a list of fuctions that will be called by cacheSolve

makeCacheMatrix <- function(x = matrix()) {
    
    ## Initializing the cached variable
    inv <- NULL
    
    ## Declaring functions that will be used by cacheSolve
    
    ## These functions set and return the inverse
    setInv <- function(inverse) {inv <<- inverse}
    getInv <- function()        {inv}
    
    ## These functions set and return the matrix itself
    set    <- function(y)       {x <<- y} 
    get    <- function()        {x}   
    
    ## A list of all the functions that will be returned
    list(x = x, getInv = getInv, setInv = setInv, set = set, get = get)
}


## Write a short comment describing this function
## cacheSolve checks to see if an inverse exists and if the matrix is identical.
## If that's all true, then it returns the cached value, 
## otherwise it computes a new inverse.

cacheSolve <- function(x, ...) {
    
    ## Bring matrices and inv into function
    inv <- x$getInv()
    newMat <- x$x
    oldMat <- x$get()
        
    ## Check to see if matrix is identical and inverse already exists
    if (identical(newMat, oldMat) & !is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    
    ## Saving inverse of newMat into inv
    inv <- solve(newMat)
    
    ## Saving inv
    x$setInv(inv)
    
    ## Saving the matrix 
    x$set(newMat)
    
    ## Return inv
    inv

}
