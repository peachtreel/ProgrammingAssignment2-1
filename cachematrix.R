makeCacheMatrix <- function (x=matrix()) {
# x is a matrix here, in the example x was a numeric vector
# this function will create a matrix than it can cache its inverse
  inv <- NULL
# inv <- NULL allows us to cache the inverse of the matrix
  set <- function(y){
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setInverse <- function(solveMatrix) inv <<- solveMatrix
  getInverse <- function() inv
  list(set = set, 
  	  get = get, 
  	  setInverse = setInverse, 
  	  getInverse = getInverse)
}


## The next function works to create the inverse of the matrix above... if the inverse was previously calculated the cachesolve will retrieve this info. 

cacheSolve <- function(x, ...) {
  ## Returns a matrix that is the inverse of matrix 'x'
  inv <- x$getInverse()
  if(!is.null(inv)){
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data)
  x$setInverse(inv)
  inv      
}