## This functions are part of the second assignment for the R Programming Language Track in Coursera




## The makeCacheMatrix function receives a matrix and return a list that contains 4 functions:
## setMatrix: let us change the value of the Matrix
## getMatrix: get the value of the matrix
## setMatrixInverse: set the value of the matrix inverse
## getMatrixInverse: get the value of the matrix inverse

makeCacheMatrix <- function(x = matrix()){
    matrixInverse <- NULL
    
    setMatrix <- function(matrix){
        x <<- matrix
        matrixInverse <<- NULL
    }
    
    getMatrix <- function() x
    setMatrixInverse <- function(matrix) matrixInverse <<- matrix
    getMatrixInverse <- function() matrixInverse
    
    list(setMatrix = setMatrix, getMatrix = getMatrix, setMatrixInverse = setMatrixInverse, getMatrixInverse = getMatrixInverse)
}



## The cacheSolve function uses a list generated with makeCacheMatrix and check for the cached value of the matrix inverse.
## If there's a cached value it is inmediately returned; otherwise the function calculate the matrix inverse, cache it and the return it.

cacheSolve <- function(cacheMatrix, ...){

    matrixInverse <- cacheMatrix$getMatrixInverse()
    
    if(!is.null(matrixInverse)){
        message("getting cached data");
        return(matrixInverse)
    }
    
    matrix <- cacheMatrix$getMatrix()
    matrixInverse <- solve(matrix,...)
    cacheMatrix$setMatrixInverse(matrixInverse)
    matrixInverse
}

