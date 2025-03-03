## An overall description of what the functions do

## This script contains two functions: makeCacheMatrix and cacheSolve.
## The makeCacheMatrix function creates a special "matrix" object that 
## can cache its inverse. The cacheSolve function computes the inverse of
## the special "matrix" returned by makeCacheMatrix. If the inverse has 
## already been calculated and the matrix has not changed, cacheSolve 
## retrieves the cached inverse instead of computing it again.

## Comment describing the makeCacheMatrix function

## The makeCacheMatrix function creates a special "matrix" object that 
## can cache its inverse. It consists of four functions:
## 1. set(): Sets the value of the matrix.
## 2. get(): Gets the value of the matrix.
## 3. setInverse(): Stores the inverse of the matrix in the cache.
## 4. getInverse(): Retrieves the cached inverse of the matrix.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  
  # Function to set a new matrix
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  
  get <- function() x
  setInverse <- function(inverse) inv <<- inverse
  getInverse <- function() inv
  
  list(set = set, get = get, setInverse = setInverse, 
       getInverse = getInverse)
}

## A short comment describing the cacheSolve function

## The cacheSolve function computes the inverse of the special "matrix"
## returned by makeCacheMatrix. If the inverse has already been calculated 
## (and the matrix has not changed), then cacheSolve retrieves the inverse 
## from the cache to avoid redundant computations.

cacheSolve <- function(x, ...) {
  inv <- x$getInverse()
  
  if (!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  
  data <- x$get()
  
  if (nrow(data) == 0){
    stop ("Matrix is empty")
  }

  inv <- solve(data, ...)
  x$setInverse(inv)
  
  inv
}
