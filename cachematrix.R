## For this assignment, assume that the matrix supplied is always invertible.

## makeCacheMatrix() receives a matrix as a parameter, if the function is 
## invoked without a parameter, it generates a matrix [1,1] with the value NA.
## The function  creates a special "matrix" which is a list containing a 
## function to:
## 1. set - set the value of the matrix and set the inverse matrix as NULL
## 2. get - get the value of the matrix
## 3. setInv - set the value of the inverse matrix
## 4. getInv - get the value of the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
    matrixInv <- NULL
    set <- function(y){
        x <<- y
        matrixInv <<- NULL
    }
    get <- function() x
    setInv <- function(Inv) matrixInv <<- Inv
    getInv <- function() matrixInv
    list(set = set, get = get,
         setInv = setInv,
         getInv = getInv)
}

## cacheSolve() receives a "matrix" special created by the function makeCacheMatrix().
## The function calculates the inverse matrix of the matrix given as parameter, 
## nevertheless, it first checks to see if the inverse matrix has already 
## been calculated. If so, it gets the inverse matrix from the cache and skips  
## the computation. Otherwise, it calculates the inverse matrix of the matrix and 
## sets the value of the inverse matrix in the cache via the setInv function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    matrixInv <- x$getInv()
    if(!is.null(matrixInv)) {
        message("Getting cached data")
        return(matrixInv)
    }
    matrix <- x$get()
    matrixInv <- solve(matrix, ...)
    x$setInv(matrixInv)
    matrixInv
}
