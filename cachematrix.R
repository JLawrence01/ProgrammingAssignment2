## These functions create a matrix along with a cache that allows us to save its inverse after it is computed once

##This function returns a list of functions that allows the user to set and retrive the values of a matrix and its inverse. 

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL #when new matrix is entered, clears any cached inverse values
  set <-function(y){ #y is a user defined matrix
    x <<- y 
    inv <<- NULL #when the matrix is changed, clears any cached inverse values
  }
  get <- function() x #returns the current matrix
  setinv <- function(inverse) inv <<- inverse  #saves a calculated inverse in the cache
  getinv <- function() inv #retreives a pre-calculated inverse from the cache
  list(set = set, get = get, setinv = setinv, getinv = getinv) #the function produces a list of functions 
}

## for a matrix defined by the function makeCacheMatrix this function will return the inverse. If the inverse has 
## already been calculated it will be looked up in the cache, otherwise it will be computed using the solve function

cacheSolve <- function(x, ...) {
  inv <- x$getinv()
  if(!is.null(inv)){
    message("getting cached data")
    return(inv) #if the inv is already known then look it up
  } #if the inv is not known, the compute it
  data <- x$get() # pull in the original matrix 
  inv <- solve(data,...) #compute the inverse
  x$setinv(inv) #save inverse to the cache
  inv #return the inverse
}
