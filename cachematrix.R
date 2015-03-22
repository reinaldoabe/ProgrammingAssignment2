## REINALDO ABE 22/03/2015
## makeCacheMatrix generates a special matrix with the necessary functions
## accountable for caching the inverse of the matrix 

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  w <- NULL
  set <- function(y) {
    x <<- y
    w <<- NULL
    m <<- NULL
  }
  get <- function() x  
  setMatrix <- function(matrix) w <<- matrix  
  getMatrix <- function() w
  list(set = set, get = get,       
       setMatrix = setMatrix, 
       getMatrix = getMatrix)
}


## cachesolve verifies if the cache is empty, if not it returns the cache for the inverse
## Otherwise it calculates the inverse and sets the cache for the following execution

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    mS <- x$getsolve()
    mM <- x$get()  
    if(!is.null(mS)) {
      message("getting cached data")
      return(mS)    
    }
    data <- x$get()  
    m <- solve(data)
    x$setsolve(m)  
    m
}
