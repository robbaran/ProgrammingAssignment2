## Put comments here that give an overall description of what your
## functions do

## makeCacheMatrix returns a list ("special vector") of functions that set/get the value of matrix x and its inverse (through setinv/getinv)

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL         ##initialize i as inverse of x
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinv <- function(inv) i <<- inv
  getinv <- function() i
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## cacheSolve returns the inverse of the matrix stored in x by :
## looking it up in the cache first, and recomputing it only if the lookup is not possible
## NOTE: x is a "special vector" as returned by makeCacheMatrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  i <- x$getinv()   ##attempt to get inverse in cache
  if(!is.null(i)) {
    message("getting cached inverse")
    return(i)
  }
  ## if inverse is not in the cache:
  M <- x$get()  ## get the matrix in "special vector" x
  i <- solve(M) ## compute inverse of the matrix
  x$setinv(i)   ## cache the computed inverse into "special vector" x
  i             ## return the computed inverse matrix
}
