# The makeCacheMatrix() function creates a list which
# Step 1: Sets the value of the matrix
# Step 2: Gets the value of the matrix
# Step 3: Sets the value of inverse of the matrix
# Step 4: gets the value of inverse of the matrix

makeCacheMatrix <- function(m = matrix()) {
  invert <- NULL
  set <- function(n) {
    m <<- n
    invert <<- NULL
  }
  get <- function() m
  setinverse <- function(inverse) invert <<- inverse
  getinverse <- function() invert
  list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}

#The cacheSolve() function returns the inverse of the matrix. 
#Firstly it checks if the inverse us already present in the cache. 
#If yes then it gets the result and skips the computation. 
#Otherwise it computes the inverse and sets the value in the cache.

cacheSolve <- function(m, ...) {
  invert <- m$getinverse()
  if(!is.null(invert)) {
    message("getting cached data.")
    return(invert)
  }
  data <- m$get()
  invert <- solve(data)
  m$setinverse(invert)
  invert
}
