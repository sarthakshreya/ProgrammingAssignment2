makeCacheMatrix <- function(x = matrix()) { ##This function creates a special "matrix" object that can cache its inverse.
      m <- NULL
      set <- function(y = matrix()) { ##set the value of the matrix
            x <<- y
            m <<- NULL
      }
      get <- function() x ##get the value of the matrix
      setinverse <- function(solve) m <<- solve ##set the value of the mean
      getinverse <- function() m ##get the value of the mean
      list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}

cacheSolve <- function(x = matrix(), ...) { ##This function computes the inverse of the special "matrix" returned by makeCacheMatrix
      m <- x$getinverse()
      if(!is.null(m)) {
            message("getting cached data")
            return(m)
      }
      data <- x$get()
      m <- solve(data, ...)
      x$setinverse(m)
      m
}
