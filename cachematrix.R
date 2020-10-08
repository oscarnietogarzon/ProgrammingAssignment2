## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  
#the matrix is defined and the inverse is created
  inv <- NULL
  
  #the user can define the matrix
  #that set the inverse to NULL
  setMatrix <- function(m) {
    x <<- m
    inv <<- NULL
  }
  
  getMatrix <- function() x #returns the matrix defined by the user
  
  setInverse <- function(inversa) inv <<- inversa # the inverse is define
  getInverse <- function() inv # returns the inverse if this has been defined
  
  #list asociated with the function
  list(set = setMatrix, get = getMatrix,
       setInverse = setInverse,
       getInverse =  getInverse)
  
}

## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getInverse() #saves the value that currently is available
  
  if(!is.null(inv)) #returns the inverse if it is available
  {
    message("getting catched data")
    return(inv)
  }
  
  #if not the inverse is calculated with solve
  
  data <- x$get() 
  inv <- solve(data,...)
  x$setInverse(inv)
  
  message("calculate inverse")
  inv
}
