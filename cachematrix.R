## Programming Assignment 2: Lexical Scoping

## This script computes the inverse of a square matrix. 
## The result is cached in a special "matrix" object in 
## order to avoid potentially time-consuming computations.
## It is assumed that matrix supplied is always invertible.


## This function creates a special "matrix" object that can 
## cache its inverse. 
## The returned object (a list) contains three functions:
## getMatrix() : to retrieve the matrix, 
## setInverse() : to store inverse of matrix, 
## getInverse() : to retrieve cached value for inverse of matrix
## The special object is used to feed cacheSolve function

makeCacheMatrix <- function(x = matrix()) {
    ## Matrix x is initalized as a function argument.
    ## Initialization of i. This is the cache for the inverse value 
    ## for matrix x. 
    i <- NULL
    
    ## Object's setter method. Leave commented to avoid further 
    ## modification of data that can invalidate cached result
    ##set <- function(y) {
    ##    x <<- y
    ##    i <<- NULL
    ##}
    
    ## Object's method for retrieving matrix data
    getMatrix <- function() x
    
    ## Object's method for storing inverse value for matrix
    ## <<- operator is used to assign a value to an object in an environment 
    ## that is different from the current environment
    setInverse <- function(inverseValue) i <<- inverseValue
    
    ## Object's method for retrieving inverse value for matrix
    getInverse <- function() i
    
    ## Build object to be returned. No setter method included here.
    list(getMatrix = getMatrix, 
         setInverse = setInverse, 
         getInverse = getInverse)
}

## This function computes the inverse of the special "matrix" object
## returned by makeCacheMatrix. If the inverse has already 
## been calculated (and the matrix has not changed because there is no setter 
## method defined inside special object returned by makeCacheMatrix), 
## then the cacheSolve retrieves the inverse from the cache.

cacheSolve <- function(x, ...) {
    ## Read cache from object
    i <- x$getInverse()
    if(!is.null(i)){
        ## A value was found in the cache
        message("Getting cached data...")
        ## Return cached value without further computations
        return(i)
    }
    ## Read matrix data from object
    data <- x$getMatrix()
    ## Compute inverse of matrix
    i <- solve(data, ...)
    ## Store inverse of matrix into object. The value is now cached.
    x$setInverse(i)
    ## Return the inverse of the matrix
    i
}
