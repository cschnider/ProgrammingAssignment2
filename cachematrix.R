## Put comments here that give an overall description of what your
## functions do

## This function creates a special "matrix" object that can cache its inverse. which is really a list containing a function to
## set the value of the matrix
## get the value of the matrix
## set the value of the inverted matrix
## get the value of the inverted matrix

makeCacheMatrix <- function(x = matrix()) {
          ##initialize matrix with NULL
          s <- NULL
          set <- function(y) {
                    x <<- y
                    s <<- NULL
          }
          ##get matrix
          get <- function() x
          ##set inverse of matrix
          setsolve <- function(solve) s <<- solve
          ##get inverse of matrix
          getsolve <- function() s
          ##output list with all 4 objects
          list(set = set, get = get,
               setsolve = setsolve,
               getsolve = getsolve)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
          ## Return a matrix that is the inverse of 'x'
          s <- x$getsolve()
          ## check if s is not NULL (!)
          if(!is.null(s)) {
                    message("getting cached inverse matrix")
                    ##return chached value
                    return(s)
          } else {
                    data <- x$get()
                    s <- solve(data, ...)
                    x$setsolve(s)
                    return(s)
          }
}
