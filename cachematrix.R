# The pair of functions 'makeCacheMatrix' and 'cacheSolve' work in conjunction
# to efficiently compute the inverse of a matrix. If the inverse hasn't been
# calculated, then it is computed and cached. If it has already been calculated
# then the cached value of the inverse is returned.

# `makeCacheMatrix` creates a special "matrix", which is
# really a list containing a function to

# 1.  set the value of the matrix
# 2.  get the value of the matrix
# 3.  set the value of the inverse
# 4.  get the value of the inverse  

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


# 'cacheSolve' calculates the mean of the special "matrix"
# created with the above function. However, it first checks to see if the
# inverse has already been calculated. If so, it `get`s the inverse from the
# cache and skips the computation. Otherwise, it calculates the inverse of
# the data and sets the value of the inverse in the cache via the `setinverse`
# function.

cacheSolve <- function(x, ...) {
    # Return a matrix that is the inverse of 'x'
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
