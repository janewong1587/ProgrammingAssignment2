## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL #initialize inv
  set <- function(y) { #set the value of the matrix 
    x <<- y            #store x and inv in the enclosing environment
    inv <<- NULL
  }
  get <- function() x  #get the value of the matrix
  setinverse <- function() inv <<- x #calculate the inverse and cache it
  getinverse <- function() inv #get the value of the inverse 
  list(set = set, get = get, #return the list
       setinverse = setinverse,
       getinverse = getinverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
  inv <- x$getinverse() 
  if(!is.null(inv)) { #if the inverse is already be calculated by the "setinverse"
    message("getting cached data")
    return(inv) #then just take the data from it
  }
  data <- x$get() #if not, load the matrix first
  inv <- solve(data, ...) #and calculate the inverse
  x$setinverse(inv) #put the inverse in the cache
  inv ## Return a matrix that is the inverse of 'x'
}
