## The two functions below cache the inverse of a matrix.
## Matrix inverse operations can be quite resource intensive to process
## Hence caching the inverse of a matrix (instead of computing it several times) can be beneficial

## This function creates a special "matrix" object that can cache the matrix inverse by using the solve function
makeCacheMatrix <- function(x = matrix()) {
m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) m <<- solve
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above
## If the inverse was already calculated (and the matrix has not changed), 
## then the cacheSolve should retrieve the inverse from the cache (a "getting cached data" message will be displayed)

cacheSolve <- function(x, ...) {
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
        ## Return a matrix that is the inverse of 'x'
}
