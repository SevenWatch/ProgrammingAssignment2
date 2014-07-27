## Put comments here that give an overall description of what your
## functions do


##This function will create a special matrix object that can cache its inverse.
##It will do the following:
## -set the value of the matrix
## -get the value of the matrix
## -set the value of the inverse
## -get the value of the inverse


makeCacheMatrix <- function(x = matrix()) {
      m <- NULL
      set <- function(y) {
            x <<- y
            m <<- NULL
      }
      get <- function() x
      setmatrix <- function(solve) m <<- solve
      getmatrix <- function() m
      list(set = set, get = get,
           setmatrix = setmatrix,
           getmatrix = getmatrix)
      
}


## Write a short comment describing this function
##This function calculates the inverse of the special "matrix" created with
##the previous function. But first, it checks to see if the inverse
##has already been calculated. If it has, it gets the inverse from the
##cache and skips the computation. Otherwise, it caculates de inverse
##of the matrix and sets it in the cache with the setmatrix function.


cacheSolve <- function(x = matrix(), ...) {
        ## Return a matrix that is the inverse of 'x'
      m <- x$getmatrix()
      if(!is.null(m)) {
            message("getting cached data")
            return(m)
      }
      matrix <- x$get()
      m <- solve(matrix, ...)
      x$setmatrix(m)
      m
      
}
