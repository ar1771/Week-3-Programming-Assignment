## R course programming assignment 2
## Write a pair of functions that cache the inverse of a matrix.

## makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()){    #initializes objects
     InverseM <- NULL
     set <- function(y) {
          x <<- y
          InverseM <<- NULL
 }
     get <- function() x
     setinverse <- function(inverse) InverseM <<- inverse  #sets inverse of matrix
     getinverse <- function() InverseM	    #gets value of inverse of matrix
     list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}



## cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
     InverseM <- x$getinverse()
     if(!is.null(InverseM)) {
          message("getting cached data")
          return(InverseM)
     }
     data <- x$get()
     InverseM <- solve(data, ...)
     x$setinverse(InverseM)
     InverseM
}

