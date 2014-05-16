## The first function, makeCacheMatrix, is a cache object that stores a matrix and its inverse 
## The second function, cacheSolve, takes advantage of the cache by storing and retrieving 
## the inverse of a matrix.

# This function creates a special "matrix" object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
  # variable that stores the value of an inversed matrix
  inversedMatrix <- NULL
  
  # function that sets the value of the matrix 
  # and sets the inversedMatrix variable to NULL
  setMatrix <- function(y) {
    x <<- y
    inversedMatrix <<- NULL
  }
  
  # gets the matrix
  getMatrix <- function() { x }
  
  # sets the inverse
  setInverse <- function(inverse) { inversedMatrix <<- inverse }
  
  # gets the inverse
  getInverse <- function() { inversedMatrix }
  
  # prints the contents of an object
  list(setMatrix = setMatrix, 
       getMatrix = getMatrix,
       setInverse = setInverse,
       getInverse = getInverse)
}
# This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
# If the inverse has already been calculated (and the matrix has not changed),
# then the cachesolve should retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
  # trt to retrieve a cached inverse value
  inversedMatrix <- x$getInverse()
  
  # ----------if---------- 
  #the value exists in cache, then return it
  if(!is.null(inversedMatrix)) {
    message("getting cached data")
    return(inversedMatrix)
  }
  
  # ----------else----------
  # the value is not yet cached, so retrieve the data
  data <- x$getMatrix()
  
  # find/calculate an inverse
  inversedMatrix <- solve(data, ...)
  
  # set the calculated inverse value from the cache
  x$setInverse(inversedMatrix)
  
  # return the calculated inverse
  inversedMatrix
}
