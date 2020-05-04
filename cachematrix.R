## these functions work together to create a matrix, calculate its inverse, and cache this inverse

## makeCacheMatrix creates the a matrix object, as well as several variables. I is a variable in which the invertet matrix will be stored. 
## i must first be cleared in case a previous value was cached. 
## set is a function which is responsible for solving the inverse and storing it
## get is a function which is involved in retrieving the cached inverse 

makeCacheMatrix <- function(x = matrix) {
      i <- NULL
      set <- function(y) {
            x <<- y
            i <<- NULL
      }
      get <- function() x
      setinverse <- function(solve) i <<- solve
      getinverse <- function() i
      list(set = set, get = get,
           setinverse = setinverse,
           getinverse = getinverse)
}

## cacheSolve first calls on the get function and checks if a cached value is returned. 
## i is then reset to be the new inverse. get retrieves the data, the inverse is calculated using solve, and the data is set and printed.

cacheSolve <- function(x, ...) {
      i <- x$getinverse()
      if(!is.null(i)) {
            message("getting cached data")
            return(i)
      }
      data <- x$get()
      i <- solve(data, ...)
      x$setinverse(i)
      i
}
