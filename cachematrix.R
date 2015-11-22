## makeCacheMatrix - makes the matrix object and cache it's inverse
## cacheSolve - takes the inverse of Matrix and if the inverse has been already taken then retrives the information from Cache

## Makes the matrix object and cache's its inverse
makecacheMatrix <- function(x = matrix()){
  inv <- NULL
  set <-function(y){
    x <<- y
    inv <<- NULL
  }
  get <-function()x
  setinverse <- function(solve) inv <<- solve
  getinverse <- function() inv
  list(set = set , get= get,
       setinverse = setinverse,
       getinverse = getinverse)
}

## computes the inverse and return it or if the inverse is already there then returns it from the cache
cacheSolve <- function(x, ...){
  inv <-x$getinverse()
  if(!is.null(inv)){
    message("getting cached data")
    return(inv)
  }
  data <-x$get()
  inv <- solve(data, ...)
  x$setinverse(inv)
  inv
}