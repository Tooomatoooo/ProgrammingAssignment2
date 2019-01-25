## Put comments here that give an overall description of what your
## functions do

## Set up a new object which can cache the inverse
makeCacheMatrix <- function(x = matrix()) {
      inv <- NULL
      set <- function(y){
            x <<- y
            inv <<- NULL
      }
      get <- function() x
      setinv <- function(inverse) inv <<- inverse
      getinv <- function() inv
      list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## Check if the program has cached the inverse
## If true then return the calculated value; otherwise do the calculation
cacheSolve <- function(x, ...) {
      ## Return a matrix that is the inverse of 'x'
      inv <- x$getinv()
      if(!is.null(inv)){
            message("getting cached data")
            return(inv)
      }
      data <- x$get()
      inv <- solve(data)
      x$setinv(inv)
      inv
}
