##sample calls for a quick unit test:
## mat <- matrix(1:4, 2, 2)
## cached <- makeCacheMatrix(mat)
## cacheSolve(cached)

## first calling cachSolve() will display
## the inverse of mat
##              [,1] [,2]
## [1,]   -2  1.5
## [2,]    1 -0.5

## check it by doing solve(mat) directly
## consequent calls will display
## getting cached data, then the inverse of mat


## makeCacheMatrix, with a matrix as a parameter
## a function that creates a vector with 4 elements
## in effect a list with 4 elements:
## function set
## function get
## function setinverse
## function getinverse
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  
  ## force y as the new value of x; nullifies the cache
  ## in effect our matrix will overwritten with the value sent by the set call
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  
  ## retrieves the original object
  get <- function() x
  
  ## sets the inverse of the original object with the value sent via its parameter "inverse"
  setinverse <- function(inverse) m <<- inverse
  
  ## gets the cached inverse 
  getinverse <- function() m
  
  ## return of this function, a list of functions
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

## function cacheSolve, takes as parameter the vector created by makeCacheMatrix 
## first time called, will calculate the inverse of the initial matrix
## and set it in the cache
## next calls for the same initial matrix will just read the cache 
## and display the inverse from there
cacheSolve <- function(x = vector()) {
  ## get the inverse from the cache
  m <- x$getinverse()
  
  ## if a value is stored there, use that instead of computing it again 
  ## and get out of the function
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  
  ## no cache, then get the original object
  data <- x$get()
  
  ## calculate the inverse of the original object
  m <- solve(data)
  
  ## set it in the cache
  x$setinverse(m)
  
  ## and show it too
  m
}
