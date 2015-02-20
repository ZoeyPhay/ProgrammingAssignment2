## This "makeCacheMatrix" function creates a special "matrix". 
## It is a list containing a function to:
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the inverse of the matrix
## 4. get the value of the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  
  ## Start inverse property
  invmatrix <- NULL
  
  ## Setting matrix
  set <- function(y) {
    x <<- y
    invmartrix <<- NULL
}
  
  ## Getting matrix
  get <- function(){
          
    ## Returning the input matrix
          x}
  ## Setting inverse of the matrix
  setInverseMatrix <-function(inv) {
    
    ## store inverse
    invmatrix <<- inv
  }
  ## Getting inverse of the matrix
  getInverseMatrix <- function() {
    
    ##Returning inverse matrix
    invmatrix
  }
  ## Listing methods
  list(set = set, get = get, setInverseMatrix = setInverseMatrix, getInverseMatrix = getInverseMatrix)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
## If the inverse has already been calculated (and the matrix has not changed),
## then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  invmatrix <- x$getInverseMatrix()
  
  ## Return if the inverse has been calculated (i.e. if !is.null(m==TRUE)
  if(!is.null(invmatrix)) {
      message("getting cached data")
      return(invmatrix)
  }

## IF INVERSE HAS NOT BEEN CALCULATED

  ## Getting matrix from our object
  data <- x$get()
  
  ## Use Matrix multiplication to calculate the inverse
  m <- solve(data) %*% data

  ## For future usage: storing the inverse to object
  x$setInverseMatrix(m)

  ## Return a matrix that is inverse of 'x'
  m

}