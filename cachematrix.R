## these functions work together to create a matrix, calculate its inverse, and cache this inverse

## makeCacheMatrix creates the matrix and defines a list of variables

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

## cacheSolve calcultes the inverse if it is found to not already be cached

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
