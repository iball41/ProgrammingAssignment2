## Put comments here that give an overall description of what your
## functions do
##iball41 - Programming Assignment #2
##There are 2 functions in this program:
##  1. makeCacheMatrix - function that takes a matrix input, creates an inverse and caches the info
##  2. cacheSolve - function that takes function as input, checks to see if inverse value is in cache (retrieves from cache); otherwise, the inverse of the matrix is computed

## Write a short comment describing this function
## makeCacheMatrix - function that takes a matrix input, creates an inverse and caches the info

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setmean <- function(solve) m <<- solve
  getmean <- function() m
  list(set = set, get = get,
       setmean = setmean,
       getmean = getmean)
}


## Write a short comment describing this function
## cacheSolve - function that takes function as input, checks to see if inverse value is in cache (retrieves from cache); otherwise, the inverse of the matrix is computed
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getmean()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setmean(m)
  m
}
