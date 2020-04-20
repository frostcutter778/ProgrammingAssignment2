## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
#This function is used for creating the Cache Matrix, where the inverse of the matrix is stored.
makeCacheMatrix <- function(x = matrix()) {
                    i <- NULL
                    set <- function(y) {
                              x <<- y
                              i <<- NULL
                    }
                    get <- function() x
                    setinverse <- function(inverse) i <<- inverse
                    getinverse <- function() i
                    list(set = set, get = get,
                         setinverse = setinverse,
                         getinverse = getinverse)
}



## Write a short comment describing this function
# This function is used for calculating the inverse of the matrix, but at first checks if the inverse of the matrix is already there in the makeCacheMatrix function.
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
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

