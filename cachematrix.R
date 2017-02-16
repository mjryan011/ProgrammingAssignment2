## These two functions serve the purpose to cache an inverse of a matrix.
## That way we do not have to recalculate the inverse of the matrix over and over again.

## This function sets and gets the value of the matrix
## This function sets and gets the value of the inverse matrix

makeCacheMatrix <- function( x = matrix()){
  inv <- NULL
  set <- function(y){
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse)inv <<- inverse
  getinverse <- function() inv
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
  
}




## This function checks to see if the inverse of the matrix has already been calculated.
## If it has then it prints the inverse without having to calculate. 
## If it has not calculated the inverse then it calculates the inverse and prints it.

cacheSolve <- function(x, ...){
  inv <- x$getinverse()
  if(!is.null(inv)){
    message("getting cached data.")
    return(inv)
  }
  mat <- x$get()
  inv <- solve(mat, ...)
  x$setinverse(inv)
  inv
}
