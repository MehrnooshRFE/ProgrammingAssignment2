## Define a function to create a "matrix" object that can 
## cache its inverse

makeCacheMatrix <- function(m = matrix()) {
  inv <- NULL  ## Initialize the inverse
  
  ## Define a function to set the value of the matrix object
  set <- function(matrix) {
    m <<- matrix  ## Assign the value of matrix to m in the parent environment
    inv <<- NULL  ## Reset the inverse to NULL
  }
  
  ## Define a function to get the value of the matrix object
  get <- function() m
  
  ## Define a function to set the inverse of the matrix object
  setInverse <- function(inverse) inv <<- inverse
  
  ## Define a function to get the inverse of the matrix object
  getInverse <- function() inv
  
  ## Define a function to compute the inverse of the matrix object and cache the result
  computeInverse <- function() {
    if(!is.null(inv)) {
      message("getting cached inverse")
      return(inv)
    }
    inv <- solve(m)  ## Compute the inverse of the matrix
    setInverse(inv)  ## Cache the inverse
    inv
  }
  
  ## Return a list of functions
  list(set = set,
       get = get,
       setInverse = setInverse,
       getInverse = getInverse,
       computeInverse = computeInverse)
}


## Define a function to compute the inverse of a matrix 
## object created using makeCacheMatrix


cacheSolve <- function(matrix) {
  inv <- matrix$getInverse()  
  ## Retrieve the current inverse of the matrix object
  
  if(!is.null(inv)) {  ## If the inverse is already cached, return it
    message("getting cached inverse")
    return(inv)
  }
  
  inv <- matrix$computeInverse()  ## Compute the inverse of the matrix object using computeInverse
  inv  ## Return the inverse
}
