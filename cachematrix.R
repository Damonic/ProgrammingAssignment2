## Takes a pre-defined matrix and finds its inverse. This inverse 
## value is saved in cache such that it can be recalled without having 
## to recalculate the inverse. 


## Takes the pre-defined matrix specified in the argument and gets the inverse from cache

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setsolve <- function(solve) m <<- solve
  getsolve <- function() m
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
}


## calculates the inverse of the matrix and if the inverse is already in cache, returns the cached value rather than recalculating

cacheSolve <- function(x, ...) {
  m <- x$getsolve()
  if(!is.null(m)) { ## checks if m is NULL or already exists in cache.  
    message("getting cached data") ## if m is in cache retreives value of m and prints message 
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setsolve(m)
  m
  ## Return a matrix that is the inverse of 'x'
}

