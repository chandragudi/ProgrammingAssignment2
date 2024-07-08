### code for Peer reviewed Assignment:  Casching the inverse of a matrix
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  
  # Function to set the matrix
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  
  # Function to get the matrix
  get <- function() {
    x
  }
  
  # Function to set the inverse of the matrix
  setInverse <- function(inverse) {
    inv <<- inverse
  }
  
  # Function to get the inverse of the matrix
  getInverse <- function() {
    inv
  }
  
  # Return a list of the above functions
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}

#Code for casche solve
cacheSolve <- function(x, ...) {
  # Get the inverse from the cache
  inv <- x$getInverse()
  
  # If the inverse is already calculated, return it
  if (!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  
  # Otherwise, compute the inverse
  data <- x$get()
  inv <- solve(data, ...)
  
  # Cache the inverse
  x$setInverse(inv)
  
  # Return the inverse
  inv
}

#Example
# Create a special "matrix" object
my_matrix <- makeCacheMatrix(matrix(c(1, 2, 3, 4), 2, 2))

# Compute the inverse and cache it
inv1 <- cacheSolve(my_matrix)
print(inv1)

# Get the cached inverse
inv2 <- cacheSolve(my_matrix)
print(inv2)

# Change the matrix
my_matrix$set(matrix(c(2, 3, 4, 5), 2, 2))

# Compute the new inverse and cache it
inv3 <- cacheSolve(my_matrix)
print(inv3)

