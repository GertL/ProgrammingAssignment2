## makeCacheMatrix creates a special matrix object, and then cacheSolve 
## calculates the inverse of the matrix.
## If the matrix inverse has already been calculated, it will instead 
## find it in the cache and return it, and not calculate it again.

makeCacheMatrix <- function(x = matrix()) {
#Setting the inversion to null
  inv_x <- NULL
  #Setting the matrix
  set <- function(y) {
    x <<- y
    inv_x <<- NULL
  }
  #Getting the matrix
  get <- function() x
  #Setting the inverse
  setinverse<- function(inverse) inv_x <<-inverse
  #Getting the inverse
  getinverse <- function() inv_x
  #Returning a list containing values
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## The function cacheSolve returns the inverse of a matrix A created with
## the makeCacheMatrix function.
## If the cached inverse is available, cacheSolve retrieves it, while if
## not, it computes, caches, and returns it.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv_x <- x$getinverse()
  #Checking if the inverse has already been calculated
  if (!is.null(inv_x)) {
    #If it is this step will get it from the cache and will not recalculate it.
    message("getting cached inverse matrix")
    return(inv_x)
  } else {
    #If it is not it will calculate it
    inv_x <- solve(x$get())
    #Next step will set the value of the inverse in the cache using the setinverse function.
    x$setinverse(inv_x)
    #Returns result
    return(inv_x)
  }
}
