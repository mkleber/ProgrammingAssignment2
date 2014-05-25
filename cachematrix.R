## Function makeCacheMatrix creates a matrix and 
## functions do

## First m is set to NULL inside the makeCachematrix function. Set is defined as
# a function that assigns the matrix y to the matrix x (from the parent 
# environment). As there is a new matrix the cached inverse matrix m is set to NULL
# The function get is the reverse and loads the matrix x. 
# The function setinverse is used to assign the function parameter 'inverse' to
# the matrix m (which is outside the setinverse function in the parent environment)
# The getinverse function just returns the inverse matrix m.

makeCacheMatrix <- function(x = matrix()) {

      m <- NULL
      set <- function(y) {
            x <<- y
            m <<- NULL
      }
      get <- function() x
      setinverse <- function(inverse) m <<- inverse
      getinverse <- function() m
      list(set = set, get = get,
           setinverse = setinverse,
           getinverse = getinverse)
      
}



# first the getinverse function is called to try to load an already existing 
# (= cached) inverse matrix.
# If m is not NULL, that means that there really is a cached matrix, this is
# printed and the cached matrix is returned by the cacheSolve function.
# Only if m is NULL, that means there was no inverse matrix cached, the matrix is
# loaded into data and the inverse matrix is computed using the solve function.
# The inverse matrix assigned to m is then stored in cache by the setinverse 
# function. Finally the last m lets the cacheSolve function return m.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
      
      m <- x$getinverse()
      if(!is.null(m)) {
            message("getting cached inverse matrix")
            return(m)
      }
      data <- x$get()
      m <- solve(data, ...)
      x$setinverse(m)
      m
      
}
