  ## makeCacheMatrix functions creates a special matrix object that can cache its inverse.
  
  
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
  


  ## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
  ## If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.
  ## If this is not the case, casheSolve will calculate the inverse of the "matrix", cache it and return it.
  
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
