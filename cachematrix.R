## Functions in this file cache the inverse of a matrix.


## This function takes an argument x of type matrix.
## And returns a list of following 4 functions 
## 1. set that sets the value of matrix x
## 2. get that gets the value of matrix x
## 3. setinverse that sets the inverse of matrix x
## 4. getinverse that gets the inverse of matrix x

makeCacheMatrix <- function(x = matrix()) {
  
  m <- NULL
  
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  
  get <- function() x
  
  setinverse <- function(inverse) m <<- inverse
  
  getinverse <- function() m
  
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
  
  ## Return a matrix that is the inverse of 'x'
  
  m <- x$getinverse()
  
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  
  data <- x$get()
  
  m <- solve(data)
  
  x$setinverse(m)
  
  m  
  
}
