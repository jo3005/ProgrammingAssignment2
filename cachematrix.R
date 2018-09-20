## This takes a inversable matrix as an input and checks
## whether a cached inverse is available for that matrix before
## calculating the inverse.  Therefore the inverse is done if the 
## input matrix has changed or if the cache is empty.

## This creates a special type containing a matrix and its inverse
## and defines functions to access the matrix/inverse components.

makeCacheMatrix <- function(mat = matrix()) {

  inv <- NULL
  setmat <- function(y) { ## set the value of the special matrix and assign its inverse to zero initially
      mat <<- y
      inv <<- NULL
  }
  getmat <- function() mat      ## pass on just the value of the matrix
  setinv <- function(solve) {inv <<- solve} ## determine the function that calculates the inverse
  getinv <- function() inv    ## pass on the just the inverse
  list(setmat=setmat,
       getmat=getmat,
       setinv=setinv,
       getinv=getinv)
  
}


## Write a short comment describing this function

cacheSolve <- function(mat, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- mat$getinv()  ## define the inverse as the getinv function of the input
  if(!is.null(inv)) { 
    message("getting cached data")  ## identify that you have a cached value and will retrieve it
    return(inv)
  }
  else {  ## don't have an existing cached value so calculate the inverse of the matrix
    data <- mat$getmat()     
    inv <- solve(data, ...)
    mat$setinv(inv)
  }
  inv
}
