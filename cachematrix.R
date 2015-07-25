## The functions below will calculate and cache the inverse of a matrix
## This way, the inverse can be calculated faster if it was calculated before

## A matrix wrapper that support inverse caching
makeCacheMatrix <- function(the_matrix = matrix()) {
  cached_matrix_inverse <- NULL
  
  # Resetting the list if needed
  set <- function(new_matrix) {
    the_matrix <<- new_matrix
    cached_matrix_inverse <<- NULL
  }
  get <- function() the_matrix
  
  # Setter and getter for the inverse
  set_matrix_inverse <- function(new_matrix_inverse) cached_matrix_inverse <<- new_matrix_inverse
  get_matrix_inverse <- function() cached_matrix_inverse
  
  # Return the list of functions to work with
  list(set = set, get = get,
       set_matrix_inverse = set_matrix_inverse,
       get_matrix_inverse = get_matrix_inverse)
}


## A Solve function (Matrix inverse) that also cache the results
cacheSolve <- function(the_matrix, ...) {
  
  # Return the cached inverse if the inverse is already calculated
  matrix_inverse <- the_matrix$get_matrix_inverse()
  if(!is.null(matrix_inverse)) {
    message("getting cached data")
    return(matrix_inverse)
  }
  
  # Calculate the inverse if it hasn't been cached
  data <- the_matrix$get()
  matrix_inverse <- solve(data)
  
  # Cache the result to the matrix object
  the_matrix$set_matrix_inverse(matrix_inverse)
  
  # Return the invered matrix
  matrix_inverse
}
