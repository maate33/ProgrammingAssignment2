## Put comments here that give an overall description of what your
## functions do

## These functions are written to return the inverse of a matrix for the R Programming course Week 3 Assignment

## This function creates an object which can cache the inverse

makeCacheMatrix <- function(x = matrix()) {
      invrs <- NULL                              
      set <- function(y) {                     
      x <<- y                            
      invrs <<- NULL                       
      }
      get <- function() x                     
      
      setinverse <- function(inverse) invrs <<- inverse  
      getinverse <- function() invrs                     
      list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)  
                                                                                    

}


## This function calculates the inverse of the object made by the makeCacheMatrix function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  invrs <- x$getinverse()
      if(!is.null(invrs)) {
            message("getting cached data")
            return(invrs)
        }
      data <- x$get()
      invrs <- solve(data, ...)
      x$setinverse(invrs)
      invrs
  
}
