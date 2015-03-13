## makeCacheMatrix creates a special "matrix", which is really a 
## list containing a function to
##  -set the value of the matrix
##  -get the value of the matrix
##  -set the value of the inverse of the matrix
##  -get the value of the inverse of the matrix

makeCacheMatrix <- function(x = numeric()) {
  inv <- NULL  #to initialize matrix, inv to null so that this function will work the first time
  #this func sets x to y and inv to null within this function's environment
  set <- function(y) { 
    x <<- y
    inv <<- NULL
  }
  # the get function assigns a matrix to it. This function
  # is useful only when worked with the cacheSolve function.
  get <- function() x
  #this function sets the value ('inverse') to inv in the makeCacheMatrix
  # enironment. This function is useful only when CacheSolve is called second time
  # 'makeVector' frame.  
  setinv <- function(inverse) inv <<- inverse
  getinv <- function() inv
  #constructs a named list of functions within the environment in the makeCacheMatrix
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}

## cacheSolve calculates the inverse of the special "matrix" 
## created with makeCacheMatrix. 
## However, it first checks to see if the inverse has already been calculated in the getinv function. 
## If so, it gets the inverse from the cache and skips the computation. 
## Otherwise, it calculates the inverse of the data and sets the value of the inverse
## in the cache via the setinv function.

# Goes to the makeCachematrix environment and assigns the 
# 'inv' value from that environment to this one.
# If the makeCacheMatrix environment has been evaluated 
# before, the function prints the message and
# the value of inv (the cached inverse).

cacheSolve <- function(x, ...) {
  inv <- x$getinv()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  # If the inverse has never been evaluated before, 
  # puts the x-matrix into a local variable called 'data'
  # Calculate the inverse of the matrix x by calling
  # solve function on the data local variable.
  # Assign the calculated inverse to the MakeCacheMatrix
  # environment using the 'setinv' function.
  # Display the calculated inverse.
  data <- x$get()
  inv <- solve(data, ...)
  x$setinv(inv)
  inv
}
