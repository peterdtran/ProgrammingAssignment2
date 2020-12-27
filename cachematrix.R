## Put comments here that give an overall description of what your
## functions do
#Function will create a matrix that will be able to cache the matrix 
###and it's inverse

## Write a short comment describing this function
#makeCacheMatrix is setting the finding the matrix argument and setting
#it's inverse to null 
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL 
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() {x} #to get matrix
  setInverse <- function(inverse) {inv <<- inverse}
  getInverse <- function() {inv} #these function wil lget the matrix inverse
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}

## Write a short comment describing this function
## cacheSolve function retrieves the cached data
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getInverse()
  if(!is.null(inv)){  
    message("getting cached data")
    return(inv)   #return inverse value
  }
  mat <- x$get()
  inv <- solve(mat, ...) #calculate inverse values
  x$setInverse(inv)
  inv ##returning a matrix inverse 
}
