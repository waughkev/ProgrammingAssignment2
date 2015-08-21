## These functions will take a matrix and store the matrix, along with creating an inverse of
## the matrix and store it as well. This is used for Coursera's R Programming Course. 
##Class Number: rprog-031 Assignment#: Assignment 2


##makeCacheMatrix function needed: This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  ## variable i that will be NULL to start, but used later
  i <- NULL
  ##setting matrix
  set <- function(y) {
    y <<- x
    i <<- NULL
  }
  ##get matrix function
  get <- function() x
  ##setinverse function that will create an inverse of the matrix
  setinverse <- function(solve) i <<- solve ## sets i as an inverse of x
  ##getinverse returns i
  getinverse <- function() i
  ##use list( to coerce functions to names)
  list(set = set, get = get,
       setinverse = setinverse, getinverse = getinverse)
}


##cacheSolve This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
##If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should 
##retrieve the inverse from the cache

#solve(x) may be needed

cacheSolve <- function(x,...) {
  ## Return a matrix that is the inverse of 'x', this will use all aspects of makeCacheMatrix
  i <- x$getinverse() #returns inverse from makeCacheMAtrix function and assigns it to i
  #if statement to determine if value is already cached
  if (!is.null(i)) {
    message('Getting cached inverse')
    return(i)
  }
  #if m is Null, x the matrix will be solved to its inverse and stored
  data <- x$get() ##assigns the variable data the value of get, which will contain the original matrix
  i <- solve(data,...) ##i becomes the inverse of data
  x$setinverse(i) ##i is then sent to x and becomes the result of the setinverse function
  i ##i all by itself
}