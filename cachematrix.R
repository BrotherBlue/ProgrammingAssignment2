## Script by AJSmith for Coursera R Programming Assignment 2
## Dec 2015

## Overall, these two functions can cache the inverse of a matrix, which is a time-consuming calculation.
## To use this cache:
##  1. Assign the list output from  makeCacheMatrix() in a new variable, say "a": (a<-makeChacehMatrix())
##  2. Store the desired matrix x using "set":  a$set(x)
##  3. At any time, get inverse of x by calling  cacheSolve function with variable a as argument: cacheSolve(a)
##  If the stored matrix has not changed, the cached inverse is retrieved, otherwise a new inverse is calculated and stored

## The makeCacheMatrix function is used to store the matrix x and its inverse for later retrieval
## The function returns a list of matrices:
##  "set" to store a matrix
##  "get" to retrieve a matrix
##  "setinverse" to store a matrix inverse
##  "getinverse" too retrieve a matrix inverse

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL  ## initialise inverse to NULL when function called - indicates no inverse matrix cached
  set <- function(y) {
    x <<- y  ## store the provided matrix in the parent environment
    inv <<- NULL  ## reset stored inverse to NULL as nothing for new matrix yet
  }
  get <- function() x  ## retrieve & return the stored matrix
  setinverse <-function(inver) inv <<- inver  ## store the provided inverse matrix in the parent environment
  getinverse <- function() inv  ## retrieve & return the stored inverse matrix
  list(set=set, get=get, setinverse = setinverse, getinverse = getinverse)  ## return is a list of functions
}


## The cacheSolve function checks the value held in makeCacheMatrix to determine if the matrix has
## changed or not, and correspondingly retrieves the cached inverse value or calculates and stores
## the new inverse.
## The function returns a matrix that is the inverse of the one set in makeCacheMatrix

cacheSolve <- function(x, ...) {
  inv <- x$getinverse()  ## obtain the stored inverse matrix
  if(is.null(inv)) {  ## if stored inverse is NULL, need to calculate new inverse
    data <-x$get()  ## obtain the stored matrix
    inv <- solve(data,...)  ## calculate inverse matrix
    x$setinverse(inv) ## store (cache) new inverse matrix for later use
  }else{  ## if stored inverse was not NULL
    message("getting cached data")  ## notify user that cached inverse is being used
  }
  inv  ## return inverse matrix, either the retreived version or newly calculated version
}
