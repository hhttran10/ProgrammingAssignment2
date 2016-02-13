## R Programming Assignment week 3
## 

## This function does 2 things: first it creates a matrix(x)
## then catches the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  invers <- NULL
  set <- function(y) {
    x <<- y
    invers <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) invers <<- inverse
  getInverse <- function() invers
  list(set = set,
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}






## This function create the inverse of the  matrix created
##in the function above. If the invers is already calculated
##and the matrix did not change, this function will return 
##the cached inverse

cacheSolve <- function(x, ...) {
  
  invers <- x$getInverse()
  if (!is.null(invers)) {
    message("getting cached data")
    return(invers)
  }
  matr <- x$get()
  invers <- solve(matr, ...)
  x$setInverse(invers)
  invers
}





