## Function to support Inversion of Matrix and cache the result
## (Inverted Matrix) to be used later.

## This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  
  inv <- NULL
  
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  
  get <- function()
    x
  
  setInverse <- function(inverse)
    inv <<- inverse
  
  getInverse <- function()
    inv
  
  list(
    set = set,get = get,setInverse = setInverse, getInverse = getInverse
  )
}


## This function returns Inverse of a square Matrix. If the Inversion fo Matrix
## is already available in cache rturns the same value. If not calculates the 
## Inversion of the Matrix, saves it in cache and returns it.
## This function assumes that the matrix supplied is always invertible.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getInverse()
  if(!is.null(inv)){
    message("getting cached data")
    return (inv)
  }
  mat<-x$get()
  inv <- solve(mat,...)
  x$setInverse(inv)
  inv
}

