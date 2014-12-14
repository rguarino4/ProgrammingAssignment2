## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

## Following the example of caching a mean, this function
## first create a "special" matrix that makes possible to 
## cache calculation.
makeCacheMatrix <- function(x = matrix()) {
  
  inv <- NULL
  
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  
  get <- function() { x }
  setinv <- function(inv1) { inv <<- inv1 }
  getinv <- function() { inv }
  
  list(set = set, get = get,
       setinv = setinv, 
       getinv = getinv)
  
}


## Write a short comment describing this function

## Following mean example, this function first looks 
## if there's some previous inverted matrix cached to  
## make a decision if it's needed to make a new matrix inversion 
## or simply uses cached inverted matrix.
cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
  
    ## First, try get inverted matrix from the cache.
    m <- x$getinv()
    ## If there's cache, than print and finish.
    if (!is.null(m)) {
      message("getting cached data")
      return(m)
    }
    
    ## If NOT, makes a new inverted matrix calculation and then put in cache.
    data <- x$get()
    m <- solve(data)
    x$setinv(m)
  
    m
}


