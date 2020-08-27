
## The function makeCacheMatrix receives a matrix x as parameter and it creates an 
## attribute inverse as null, which will contain the inverse of the matrix once it is set by the setInverse function

makeCacheMatrix <- function(x = matrix()) {
  ## The inverse of the matrix is initially set to null
  inverse <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  ## Save the inverse Matrix in the variable inverse
  setInverse <- function(inverseM) inverse <<- inverseM
  getInverse <- function() inverse
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## The cacheSolve function return the inverse of a matrix x that receives as a parameter. 
## If the inverse is already calculated, then it return just the value that is already store
## Otherwise it calculate the inverse of the matrix

cacheSolve <- function(x, ...) {
  ##get the value of the inverse Matrix of X 
  inverse <-x$getInverse()
  ##if X already have an inverse. It is shown to the user. Otherwise the inverse is calculated
  if(!is.null(inverse)){
    message("getting cached data")
    return(inverse)
  }
  data <- x$get()
  ##calculate the inverse of the matrix
  inverse <- solve(data,...)
  x$setInverse(inverse)
  inverse
}
