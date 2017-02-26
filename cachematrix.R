## Programming Assignment 2 - R Caching the INverse of a Matrix
## The functions below are used to create a matrix and invert it. When a 
## matrix is inverted it is cached and the cached version is retrieved first.

## To run the functions you first need to create a new object as the makeCacheMatrix function.
## Next you can set the matrix calling the set() function.
## From here you can call the cacheSolve() function and pass the makeCacheMatrix object you created earlier
## into the function.
## The first time this function is called, the matrix will be inverted and cached.
## The next time you call the cacheSolve function, you will get back the cached copy of the matrix.

## The makeCachedMatrix function receives a matrix as an argument
## and sets the inverseMatrix vector to NULL.
makeCacheMatrix <- function(mMatrix = matrix()) {
  # instantiate an inverseMatrix variable as NULL
  inverseMatrix <- NULL
  
  # Create a set function to set the mMatrix
  set <- function(mNewMatrix) {
    #set the original mMatrix to nNewMatrix
    mMatrix <<- mNewMatrix
    #set the inverseMatrix to NULL
    inverseMatrix <<- NULL
  }
  
  # Create the get function to retrieve the value of the mMatrix
  get <- function() mMatrix
  
  # Create the function to set the inverse of the Matrix
  setInverse <- function(mInv) inverseMatrix <<- mInv
  
  # Create the function to get the inverse of the matrix
  getInverse <- function() inverseMatrix
  
  # Return the list of the functions for the object
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## The cacheSolve function accepts the makeCacheMatrix object
## and tests to see if there is a cached version of the inverse of the matrix
## and if there is not, it will create the inverse and then call the 
## makeCacheMatrix object's setInverse method to set cache the inverted matrix.
cacheSolve <- function(objMatrix, ...) {
  ## Return a matrix that is the inverse of 'objMatrix'
  
  ## try to get the cached invers of the matrix
  m <- objMatrix$getInverse()
  
  ## test to see if the value was null or if it was a value then it was
  ## cached. If it was cached, write a message indicating you are getting
  ## cached data, and return the matrix and get out of the function.
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  ## get the original matrix held in the makeCacheMatrix object.
  data <- objMatrix$get()
  
  ## call the solve functin to get the inverse of the matrix
  m <- solve(data, ...)
  
  ## set the inverse varable of the ojbect
  objMatrix$setInverse(m)
  
  ## return the inverse of the matrix
  m
}
