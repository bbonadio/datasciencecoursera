## Both functions below should be used together. The first (makeCacheMatrix) creates a matrix object
## with different functions to get the value of the matrix or its inverse.
## The second function can be used to retrieve the inverse of a matrix created
## with makeCacheMatrix

## makeCacheMatrix(x) takes a matrix as argument
## and returns a list of functions that it associates to the matrix
## to get the value of the matrix, set the value, set the value of the inverse
## or get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL
  
  #When setting the matrix or changing it using the function set, we attribute
  #the new value to the matrix, and make the inverse NULL because it would have
  #to be calculated again
  set <- function(y) {
    x<<-y
    inverse<-NULL
  }
  
  #create a function get that just returns the matrix
  get = function() x
  #create a function setInverse that replace the variable inverse with the argument
  setInverse = function(inv) inverse <<- inv
  #create a function getInverst that just returns the stored variable inverse
  getInverse = function() inverse
  
  #return
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
  
}


## cacheSolve(x, ...) takes a matrix object created with the makeCacheMatrix command 
## and returns the inverse by first looking at the cache, and if necessary computing it

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  
  #get the possibly cached value of the inverse
  invMatrix <- x$getInverse()
  #if the cached value is not NULL, it means that
  #it contains the inverse, so we can just return it
  if(!is.null(invMatrix)) {
    message("getting cached inverse")
    return(invMatrix)
  }
  
  #otherwhise, if the cached value is NULL 
  #(as it is when the matrix was just set)
  #we compute the inverse by first retreiving the matric by calling the function get
  message("nonexisting cached inverse, creating the cached value")
  matrix <- x$get()
  #compute the matrix by calling slove
  invMatrix <- solve(matrix, ...)
  #cache the inverse by calling setInverse
  x$setInverse(invMatrix)
  #finally, return the inverse
  invMatrix
}
