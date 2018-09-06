## R Programming Week 3 Programming Assignment
## Coursera

## COMMANDS TO EXECUTE:
## i <- makeCacheMatrix(matrix(rnorm(16),nrow = 4,ncol = 4))
## i$get()
## i$getInv()
## cacheSolve(i)
## i$getInv()


## Function:    makeCacheMatrix
## Description: This function creates a special "matrix" object that can 
##              cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
  
  m <- NULL
  
  setInv <- function(inv){ 
    m <<- inv
  }
  
  getInv <- function() {
    m
  }
  
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  
  get <- function() {
    x
  }
  
  list(set = set,
       get = get,
       setInv = setInv,
       getInv = getInv)
}

## Function:    cacheSolve
## Description: This function computes the inverse of the special "matrix" 
##              returned by makeCacheMatrix above. If the inverse has 
##              already been calculated (and the matrix has not changed), 
##              then the cachesolve should retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
  
  m <- x$getInv()
  
  if (!is.null(m)) {
    return(m)
  }
  else {
    i <- x$get()
    m <- solve(i, ...)
    x$setInv(m)
    m
  }
}
