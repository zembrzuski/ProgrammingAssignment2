# I made this function in the same way done in the coursera example.
# This function has the same functions defined in coursera example, and
# it saves the state of the inverted matrix.
makeCacheMatrix <- function(x = matrix()) {
  # cached state for the inverted matrix
  invertedMatrix <- NULL  
  
  # when this function is invoked to change the
  # values of the matrix, it cleans the cache.
  set <- function(y) {
    x <<- y
    invertedMatrix <<- NULL
  }
  
  # returns the value of the matrix
  get <- function() {
    x
  }
  
  # change the value of the inverted matrix
  setInvertedMatrix <- function(x) {
    invertedMatrix <<- x
  }
  
  # returns the value of the inverted matrix
  getInvertedMatrix <- function() {
    invertedMatrix
  }
  
  list(set = set, get = get, setInvertedMatrix = setInvertedMatrix, getInvertedMatrix = getInvertedMatrix)
  
}


# this function verifies if the inverted matrix was already solved
# and gives the result in this case. Otherwise, it solves de inverted
# matrix and caches the matrix.
cacheSolve <- function(x, ...) {
  invertedMatrix <- x$getInvertedMatrix()
  
  # if the inverted matrix was already solved, returns the value.
  if (!is.null(invertedMatrix)) {
    message("getting cached data")
    return(invertedMatrix)
  }
  
  # if the inverted matrix was not solved yet, it will be solved now.
  data <- x$get()
  invertedMatrix <- solve(data)
  x$setInvertedMatrix(invertedMatrix)
  
  invertedMatrix
}








## now, I'll try to do it better
## here, I don't have the setInvertedMatrix function.
## Instead, it will be solved if the user calls getInvertedMatrix
## for the first time. If the user calls getInvertedMatrix other times,
## this function will return the cached value.
makeCacheMatrix2 <- function(x = matrix()) {
  invertedMatrix <- NULL  
  
  set <- function(y) {
    x <<- y
    invertedMatrix <<- NULL
  }
  
  get <- function() {
    x
  }

  # lazy load for the inverted matrix.
  # if it's already solved, just return the value
  # if it was never solved, solve it, and store the result
  # for cache.
  getInvertedMatrix <- function() {
    if (is.null(invertedMatrix)) {
      message("solving matrix and storing for cache")
      invertedMatrix <<- solve(x)      
    }
    invertedMatrix
  }
  
  list(set = set, get = get, getInvertedMatrix = getInvertedMatrix)
  
}
