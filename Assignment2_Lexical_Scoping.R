## Assignment: Caching the Inverse of a Matrix
## This pair of functions cache and compute the inverse of a matrix to avoid redundant calculations.

## makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(matrix = matrix()) {
  inverseCache <- NULL
  
  ## Function to set the matrix and clear the cached inverse
  setMatrix <- function(newMatrix) {
    if (!is.matrix(newMatrix)) {
      stop("Input must be a matrix")
    }
    matrix <<- newMatrix
    inverseCache <<- NULL
  }
  
  ## Function to get the matrix
  getMatrix <- function() {
    matrix
  }
  
  ## Function to set the cached inverse
  setInverse <- function(inverse) {
    inverseCache <<- inverse
  }
  
  ## Function to get the cached inverse
  getInverse <- function() {
    inverseCache
  }
  
  ## Return a list of functions to interact with the matrix and cached inverse
  list(setMatrix = setMatrix, getMatrix = getMatrix, setInverse = setInverse, getInverse = getInverse)
}

## cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix.
## If the inverse has already been calculated (and the matrix has not changed), 
## then cacheSolve retrieves the inverse from the cache.

cacheSolve <- function(specialMatrix, ...) {
  ## Check if the inverse is already cached
  inverseCache <- specialMatrix$getInverse()
  
  if (!is.null(inverseCache)) {
    message("retrieving cached data")
    return(inverseCache)
  }
  
  ## Get the matrix from the special "matrix" object
  matrix <- specialMatrix$getMatrix()
  
  ## Calculate the inverse and cache it
  if (nrow(matrix) != ncol(matrix)) {
    stop("Matrix must be square to calculate the inverse")
  }
  inverseCache <- solve(matrix, ...)
  specialMatrix$setInverse(inverseCache)
  
  ## Return the inverse
  inverseCache
}

## Example usage:

# Creating a 3x3 invertible matrix
exampleMatrix <- matrix(c(1, 2, 3, 0, 1, 4, 5, 6, 0), 3, 3)

# Creating a special matrix object that can cache its inverse
cachedMatrix <- makeCacheMatrix(exampleMatrix)

# Getting the matrix
print("Original Matrix:")
print(cachedMatrix$getMatrix())

# Calculating and caching the inverse of the matrix
inverse1 <- cacheSolve(cachedMatrix)
print("Inverse Matrix (first computation):")
print(inverse1)

# Retrieving the cached inverse of the matrix
inverse2 <- cacheSolve(cachedMatrix)
print("Inverse Matrix (retrieved from cache):")
print(inverse2)

# Changing the matrix
newMatrix <- matrix(c(2, 3, 4, 1, 0, 5, 6, 7, 1), 3, 3)
cachedMatrix$setMatrix(newMatrix)

# Calculating and caching the new inverse of the matrix
inverse3 <- cacheSolve(cachedMatrix)
print("New Inverse Matrix (after changing the original matrix):")
print(inverse3)

## End of the code
