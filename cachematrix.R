## makeCacheMatrix returns a list of functions that store the original and inverse of a matrix and allow it to be indexed later 
## by cacheSolve which then produces the inverse of a matrix either from stored values or performs a fresh calculation

#get places the values of the x (in the argument) in an n by n matrix.
#when the value of x in a variable changes, 'set()' re-sets that to the new values and removes any existing inverse stored along with the old values 
#'setsolve()' is enables cacheSolve to store a value of the matrix inverse 
#'getsolve()' returns the matrix inverse value
makeCacheMatrix <- function(x = matrix()) {
  #cache of matrix inverse is null to begin
  m <- NULL

  #if new matrix values are given, x takes on those values 
  set <- function(y) {

   x <<- y
   m <<- NULL
  }
  #get values to form a matrix
  get <- function() {dim(x) <- c(sqrt(length(x)), sqrt(length(x)))
                     x
  }
  
  #to allow cacheSolve to pass on a new inverse and store this value
  setsolve <- function(inverse) m <<- inverse
  
  #stored values that can be passed onto cacheSolve when it checks for cached inverse
  getsolve <- function() m
  
  #the returned values of this function: a list containing all functions that can be indexed 
 list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve )
}

#if cache is empty the function calculates the inverse, if it is not, it gets the stored value from getsolve() with a message
#once the inverse value is calculated it sets the inverse in the cache using setsolve() 
cacheSolve <- function(x, ...) {
  m <- x$getsolve()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setsolve(m)
  m
}
