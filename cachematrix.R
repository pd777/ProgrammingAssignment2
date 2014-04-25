################################################################################
## The following function creates a special "matrix", 
## which is really a list containing a function to
## - set the value of the matrix
## - get the value of the matrix
## - set the value of the inverse
## - get the value of the inverse

## Caching a matrix will enable more efficient use of memory for later
## references and demonstrates how R functions are 'top class'
## to test this you can create a matrix and assign it to myCache (for example)
## then use myCache$get() to show its contents 
#################################################################################
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  
  # to set the value of the matrix
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  # to get the value of the matrix
  get <- function() x
  # to set the inverse
  setinv <- function(inv_) inv <<- inv_
  # to get the inverse
  getinv <- function() inv
  
  # return a list of all the functions
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)     
}

################################################################################################
### cacheSolve function
### function to create the inverse of a previously cached matrix
### check if the inverse is already cached
###  x here is the name of the cache that you created in the MakeCacheMatrix function
###  the inverse then gets set back into the cache
###############################################################################################
cacheSolve <- function(x, ...) {
## Return a matrix that is the inverse of 'x'
  inv <- x$getinv()
  if(!is.null(inv)) {
    message("getting cached matrix")
    return(inv)
     }
  # if there is nothing in the matrix (cached inverse) use solve to calculate the inverse
  data <- x$get()
  inv <- solve(data)
  #  cache the inverse
  x$setinv(inv)
  inv
}