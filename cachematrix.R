## The 2 following functions aim at demonstrating how intermediate calculations can be "cached", i.e. stored in a closure in order
## to avoid multi-recalculations when it is not necessary.
## The makeCacheMatrix function creates a special matrix object capable of caching its inverse
## The cacheSolve functions computes the inverse of the special matrix returned by the makeCacheMatrix function above
## Use case :
## > makeCacheMatrix (MyMatrix)
## > cacheSolve (MyMatrix)
## When cacheSolve is called again after the first call, a message indicates that the returned result is provided by the cache
## and was therefore not recalculated.


## Function to create a special matrix object. It retains the provided matrix but also its inverse when calculated
## Use the following functions on the resulting variable :
##    - set : assigns a new matrix to the object
##    - get : displays the current matrix
##    - setinverse : stores the inverse matrix in cache
##    - getinverse : returns the inverse matrix stored in cache and NULL if no inverse matrix is yet calculated
makeCacheMatrix <- function(x = matrix()) {
   ## By default, any call to the inverse matrix should return NULL
   ## Without this initialization, a call to getinverse would raise an error if setinverse has not been previously called
   i <- NULL
   ## Internal "set" function of the object
   set <- function(y = matrix()) {
      ## Stores the new matrix in cache using "<<" and resets the associated inverse matrix
      x <<- y
      i <<- NULL
   }
   ## Internal "get" function of the object
   get <- function() {
      ## directly returns the x matrix provided when object was created or through the set function
      x
   }
   ## Internal "setinverse" function of the object
   setinverse <- function(inv = matrix()) {
      ## Stores the inverse matrix in cache 
      i <<- inv 
   }
   ## Internal "getinverse" function of the object
   getinverse <- function() {
      ## Returns the stored inverse matrix. If never stored using setinverse, returns NULL because of i <- NULL
      i   
   }
   ## Return value of the makeCacheMatrix function. It contains a set of functions that will allow the user to access
   ## the matrix value, inverse matrix value but also to set a new matrix value or to store the inverse matrix in cache
   list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}

## Function to compute the inverse of a special square matrix, created using the makeCacheMatrix function
cacheSolve <- function(x, ...) {
   ## The function first checks whether the inverse has already been calculated in cache by getting the current cached value
   i <- x$getinverse()
   ## If so, it avoids any further calculation and directly returns the previously calculated result
   if(!is.null(i)) {
      message("Retrieving cached value")
      return(i)
   }
   ## If not, computes the inverse matrix (by assumption, the provided matrix is always invertible)
   i <- solve(x$get(), ...)
   ## ... then stores the result in cache
   x$setinverse(i)
   ## ... and finally returns it.
   i
}
