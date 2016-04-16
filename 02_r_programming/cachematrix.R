## Caching the Inverse of a Matrix
## Matrix inversion is usually a costly computation and there may be some 
## benefit to caching the inverse of a matrix rather than compute it repeatedly. 
## The pair of functions below are used to create a special "matrix" object that
## caches its inverss and computes the inverse of this special "matrix" 

## This fucntion creates a special "matrix" object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
  mat_inv <- NULL
  set <- function(y) {
    x <<- y
    mat_inv <<- NULL
  }
  get <- function() x
  set_inv <- function(inverse) mat_inv <<- inverse
  get_inv <- function() mat_inv
  list(set = set, get = get,
       set_inv = set_inv,
       get_inv = get_inv)

}


## This function computes the inverse of the special "matrix" returned by 
## makeCacheMatrix above
cacheSolve <- function(x, ...) {
  mat_inv <- x$get_inv()
  if(!is.null(mat_inv)) {
    message("getting cached data")
    return(mat_inv)
  }
  mat <- x$get()
  mat_inv <- solve(mat, ...)
  x$set_inv(mat_inv)
  mat_inv
        
}


## Tests
source("cachematrix.R")

# Create a special "my_mat" matrix object to cache its inverse
my_mat <- makeCacheMatrix(matrix(rnorm(9), 3, 3))

# Return the matrix object
my_mat$get()

# This statement will return NULL since the cache to store matrix inverse is 
# empty. It hasn't been computed yet by the CashSolve() function
my_mat$get_inv()

# Solve and return inverse of the matrix and store it in cache
cacheSolve(my_mat)

# Skip solving the inverse of the matrix and return inverse stored in cache
cacheSolve(my_mat)

# Return inverse of the matrix stored in cache
my_mat$get_inv()

# Pass a new set of matric data into the "my_mat" special matrix
my_mat$set(matrix(rnorm(100), 10, 10))

# Return the matrix object
my_mat$get()

# This statement will return NULL since the cache to store matrix inverse is 
# empty. It hasn't been computed yet by the CashSolve() function
my_mat$get_inv()

# Solve and return inverse of the matrix and store it in cache
cacheSolve(my_mat)

# Skip solving the inverse of the matrix and return inverse stored in cache
cacheSolve(my_mat)

# Return inverse of the matrix stored in cache
my_mat$get_inv()