## Part 1
##The following function will calculate the matrix inverse using normal method

## Setting matrix's value
makeCacheMatrix <- function(x = matrix()) {
   i<- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  ## Getting  matrix's value
  get <- function() x
  
  ## Setting martix inverse
  setinverse <- function(solve) i <<- solve
  getinverse <- function() i
  
  ## Getting martix inverse
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}
## Part 2
##The following function will calculate the matrix inverse using cache method

## Returning a matrix that is the inverse of 'x'
cacheSolve <- function(x, ...) {
  ## Getting martix inverse 
  i <- x$getinverse()
 if(!is.null(i)) {
    message("The function ran already,pulling results from cache")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  ## setting matrix inverse
  x$setinverse(i)
  i
}


##Testing the results
## > x = rbind(c(11,99), c(-99, -11))
## > m = makeCacheMatrix(x)
## > m$get()
##      [,1] [,2]
##  [1,]   11   99
## [2,]  -99  -11
## > cacheSolve(m)
##              [,1]         [,2]
## [1,] -0.001136364 -0.010227273
## [2,]  0.010227273  0.001136364
## > cacheSolve(m)
## The function ran already,pulling results from cache
##              [,1]         [,2]
## [1,] -0.001136364 -0.010227273
## [2,]  0.010227273  0.001136364 
