## Description: 
## Matrix inversion is usually a costly computation and there may be some benefit to caching the inverse of a matrix rather than computing it repeatedly.


## makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
## Arguments:
## x    Source matrix to be processed
makeCacheMatrix <- function(x = matrix()) {
  ## Initialization
  m <- NULL
  
  
  ## SET function declaration
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  
  ## GET function declaration
  get <- function() x ## Returns 'x' (the source matrix)
  
  ## SETINVERSE function declaration
  setinverse <- function(inverse) m <<- inverse ## Setting 'm' to the inverse of the special "matrix"
  
  ## GETINVERSE function declaration
  getinverse <- function() m ## Returns 'm' (the inverse of the special "matrix")
  
  ## Generating a list corresponding to the special "matrix" to be processed
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
  
  ## Return the special "matrix" (list generated above)
}


## cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix.
## Arguments:
## x    Special "matrix"
## ...  Additional arguments to be passed to "solve" method (see R Documentation).
cacheSolve <- function(x, ...) {
        
  ## Setting 'm' to the inverse of 'x'
  m <- x$getinverse()
  
  ## Checking is 'm' is NULL
  if(!is.null(m)) {
    ## 'm' is NOT NULL so inverse is already cached...
    message("getting cached data")
    
    ##...and finally, it is returned
    return(m) 
  }
  
  ## 'm' is NULL, so inverse is not cached.
  ## Inverse of 'm' is going to be calculated/generated...
  
  ## Setting 'data' to the source matrix to be processed
  data <- x$get()
 
  ## Setting 'm' to the inverse of the source matrix
  m <- solve(data, ...)
  
  ## Setting to the special "matrix" the inverse of the source matrix
  x$setinverse(m)
  
  m ## Finally, 'm' is returned
}

## Example 1:
## my_matrix <- matrix(c(1,2,2,1),nrow=2,ncol=2)
## my_special_matrix <- makeCacheMatrix(my_matrix)
## cacheSolve(my_special_matrix)

## Example 2:
## my_special_matrix <- makeCacheMatrix(matrix(c(1,2,3,4,5,4,3,2,1),nrow=3,ncol=3))
## cacheSolve(my_special_matrix)


