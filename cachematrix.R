## Matrix inversion is usually a costly computation and their may be some benefit to caching the inverse of a matrix rather than compute it repeatedly 
## For this purpose, 2 functions are created.

## makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.


makeCacheMatrix <- function(x = matrix()) {
  
  ## Init values
  m <- NULL 
  y <- NULL
  
  ## set the value of the matrix
  setmatrix <- function(y) {
    x <<- y 
    m <<- NULL 
  }
  
  ## get the value of the matrix
  getmatrix <- function() x
  
  ## set the inverse of the matrix
  setinvertedmatrix <- function(solve) m <<- solve
  
  ## get the inverse of the matrix
  getinvertedmatrix <- function() m
  
  ## creates a list to house the four functions
  list(setmatrix = setmatrix, getmatrix = getmatrix, 
       setinvertedmatrix = setinvertedmatrix,
       getinvertedmatrix = getinvertedmatrix)
}


## cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  
  ## Check if the inverted matrix has already been calculated
  m <- x$getinvertedmatrix() 
  
  ## If already calculated, return it and it is over
  if(!is.null(m)){ 
      return(m)
    }
  
  ## Retrieve the matrix
  y <- x$getmatrix() 
  
  ## Cache the matrix
  x$setmatrix(y) 
  
  ## Calculate the invert of the matrix
  m <- solve(y, ...) 
  
  ## Cache the inverted matrix
  x$setinvertedmatrix(m) 
  
  m
}
