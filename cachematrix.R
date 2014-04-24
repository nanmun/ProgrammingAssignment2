## Functions in this file cache the inverse of a matrix.
## There is no error checking in these function, 
## It is assumed that the input matrix is invertible


## This function takes an argument x of type matrix.
## And returns a list of following 4 functions 
## 1. set function that sets the value of x
## 2. get function that gets the value of x
## 3. setinverse function that sets the value of m (used to store the inverse of the matrix x)
## 4. getinverse function that gets the value of m 

makeCacheMatrix <- function(x = matrix()) {

  ## Initialize m as null
  m <- NULL
  
  ## set x using input matrix y and reset m as null 
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  
  ## return the current value of x
  get <- function() x
  
  ## set m 
  setinverse <- function(inverse) m <<- inverse
  
  ## get m 
  getinverse <- function() m
  
  ## create and return the list of the four functions
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
  
}


## This function calculates the inverse of the matrix created with the above function. 
## It first checks to see if the inverse has already been calculated. 
## If so, it gets the inverse from the cache usgin getinverse function
## and skips the computation. 
## Otherwise, it calculates the inverse of the matrix and 
## sets the value of the inverse in the cache via the setinverse function

cacheSolve <- function(x, ...) {
  
  
  m <- x$getinverse()
  
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  
  data <- x$get()
  
  ## Calculate the inverse of x using function solve
  m <- solve(data)
  
  ## cache the inverse of x
  x$setinverse(m)

  ## return the local m which is holding the inverse of x
  m  
  
}
