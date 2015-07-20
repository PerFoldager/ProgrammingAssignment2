## This file contains two main functions 
## 1. makeCacheMatrix, which needs to be initiated first with a matrix, before the function instanse of makeCacheMatrix can be used.
## 2. cacheSolve, which calculates the inverse of the matrix that was used as input to makeCacheMatrix.
##    cacheSolve requires that it is called with the an instanse of the makeCacheMatrix function.
##    An function instanse is the result of call to makeCacheMatrix(a_matrix). 
##    See example below:
##      mcm <- makeCacheMatrix(a_matrix)
##      a_matrix_inverted <- cacheSolve(mcm)
##

## makeCacheMatrix holds a matrix and the inverse of the matrix in cache in the enclosing environment as global variables.
## An inverse matrix can only be created for a square non-singular matrix.
## The variable x holds the cached matrix and x_inverse holds the inversed matrix of x
## makeCacheMatrix(a_matrix) initiates the object and a_matrix is stored in cache as global variable x

makeCacheMatrix <- function(x = matrix()) {
  x_inverse <- NULL

  ## set receives a new matrix and replace content of x in cache and resets the inverse matrix in cache. 
  ## The operator <<- results in creating global variable in outer lexical scope.
  ## A global variable can maintain the state across function invocations.
  set <- function(y) {
    x <<- y
    x_inverse <<- NULL
  }  

  ## get the matrix that makeCacheMatrix was initiated with
  get <- function() x

  ## setInverse receives a new inversed matrix and replace the cached inversed matrix
  setInverse <- function(matrix_data) { 
      x_inverse <<- matrix_data
  }

  ## get the inversed matrix from cache
    getInverse <- function() x_inverse

  ## declare the functions we will use
  list(set = set, get = get, 
       setInverse = setInverse, 
       getInverse = getInverse) 
}


## cacheSolve calculates the inverse of a matrix and stores the inversed matrix in a makeCacheMatrix object cache.
## Input to cacheSolve is a makeCacheMatrix object, that has been initiated with a matrix
## cacheSolve ensures that the matrix in makeCacheMatrix is square non-singular matrix, which is requirement in order to calculate the inverse matrix

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  ## x must be an instanse of function makeCacheMatrix that contains the matrix we will calculate the inverse of
  ## Be aware that x in cacheSolve is a function instanse and different from the matrix x in makeCacheMatrix 

  ## First we check to ensure we have a square matrix
  if (nrow(x$get()) != ncol(x$get())) {
    stop("cacheSolve: Matrix provided is not square")
  }

  ## Second we check to ensure that the received square matrix is not singular. 
  ## We cannot calculate the inverse of a singular matrix 
  x_det <- det(x$get())
  if (x_det == 0) {
    stop("cacheSolve: Input Matrix is singular and inverse matrix cannot be calculated")
  }

  ## We check to see if we already have the inversed matrix in cache by calling getInverse in the received instance of makeCacheMatrix.
  ## If we do an inversed matrix we save the expensive calculation of the inversed matrix by returning the version from the cache in makeCacheMatrix
  inversed_matrix <- matrix()
  inversed_matrix <- x$getInverse()
  if(!is.null(inversed_matrix)) {
    message("cacheSolve getting cached data")
    return(inversed_matrix)
  }
  
  ## The inversed matrix did not exist in cache. We create the inversed matrix by using solve and store the inversed matrix in cache in makeCacheMatrix function instanse.
  inversed_matrix <- solve(x$get())
  x$setInverse(inversed_matrix)
  return(x$getInverse())
}
