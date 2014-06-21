library(MASS)

# These functions allow you to create an object w/ a matrix variable m
# this way: mc <- makeCacheMatrix(m)
# one can then obtain a CACHED version of the inverse if needed, instead of
# calculating it everytime:
# mci <- cacheSolve(mc)

# This function create an object with 'getters' and 'setters'
# (the fancy names for these are 'accessors' and 'mutators')
# in addition to getting and setting the matrix, one can get and set
# the inverse, calculated with the ginv function of the MASS package
# (generalized inverse, does not require a square matrix)

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
  
# this function will either get and return a cached inverse of the matrix
# (if it has already been calculated and stored), or will calculate and restore it
# usage:
# mci <- cacheSolve(mc)
# you should then see:
# mc %*% mci
# is the identity matrix (or really close, perhaps an off-diagonal element is something
# like 2.664535e-15)

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    m <- x$getinverse()
    if(!is.null(m)) {
      message("getting cached data")
      return(m)
    }
    data <- x$get()
    # this one, not sure
    m <- ginv(data, ...)
    x$setinverse(m)
    m
}
