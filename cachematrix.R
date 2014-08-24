## This is a function that creates a "matrix" object
## It saves the marix and its inverse into variables x and mtrxSol respectively
## It returns a list of methods:
## set, which sets matrix and resets cached inverse;
## get, which returns matrix;  setSolve whcih saves solve value;
## and getSolve which returns cached inverse value

makeCacheMatrix <- function(x = matrix()) {
        
        mtrxSol <- NULL
        
        set <- function(s) {
                
                x <<- s
                
                mtrxSol <<- NULL
                
        }
        
        get <- function() x
        
        setSolve <- function(solve) mtrxSol <<- solve
        
        getSolve <- function() mtrxSol
        
        list(set = set, get = get,  setSolve = setSolve,  getSolve = getSolve)

}


## Write a short comment describing this function
## This function gets the inverse matrix object created by makeCacheMatrix.
## Checks whether the inverse has already been cached and returns
## the cached value if it is true and if not it calculates the inverse and then returns the result.

cacheSolve <- function(x, ...) {
        
        mtrxSol <- x$getSolve()
        
        if(!is.null(mtrxSol)) {
                
                message("retreiving data from the cache")
                
                return(mtrxSol)
        }
        
        cachedData <- x$get()
        
        mtrxSol <- solve(cachedData, ...)
        
        x$setSolve(mtrxSol)
        
        mtrxSol

}