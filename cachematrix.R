## the two functions here are used to calculate and display the inverse of a given matrix input.
## the first function makeCacheMatrix sets and gets the value of the matrix aswell as that of the
## inverse of the matrix.
makeCacheMatrix <- function(x = matrix()){
      inv <- NULL
      set <- function(y){
            x <<- y
            inv <<- NULL
      }
      get <- function() x
      setInverse <- function(inverse) inv <<- inverse
      getInverse <- function() inv
      list(set = set, get = get,
 	setInverse = setInverse,
	getInverse = getInverse)
}

## the cacheSolve function is used to calculate the inverse of the given matrix.
cacheSolve <- function(x, ...){
      inv <- x$getInverse()
      if(!is.null(inv)){
            message("getting cached data")
            return(inv)
      }
      mat <- x$get()
      inv <- solve(mat, ...)
      x$setInverse(inv)
      inv
}

