## Put comments here that give an overall description of what your
## functions do

## solution for caching the inverse of a matrix 

makeCacheMatrix<-function(x = matrix()){
  
  inv_mat <- NULL
  set <- function(y) {
    x <<- y
    inv_mat <<- NULL
  }
  get <- function() x
  set_inv <- function(inverse) inv_mat <<- inverse
  get_inv <- function() inv_mat
  list(set = set, get = get,
       set_inv = set_inv,
       get_inv = get_inv)
}

## This function calculates the inverse of the matrix makeCacheMatrix

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv_mat <- x$get_inv()
  if (!is.null(inv_mat)) {
    message("getting cached data")
    return(inv_mat)
  }
  mat <- x$get()
  inv_mat <- solve(mat, ...)
  x$set_inv(inv_mat)
  inv_mat
}