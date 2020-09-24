## These functions allow to calculate the inverse of a matrix using cache in case of
## the data were previously calculated. By doing so, there are not calculations made
## twice or more. 

## This function returns a matrix than can store in cache its inverse

makeCacheMatrix <- function(m = matrix()) {
      inv_ <- NULL
      set <- function(y) {
            m <<- y
            inv_ <<- NULL
      }
      get <- function() m
      setinv <- function(inverse) inv_ <<- inverse
      getinv <- function() inv_
      list(set = set, get = get,
           setinv = setinv,
           getinv = getinv)
}

## This function receive as argument a matrix created by makeCacheMatrix function. 
## So, if the inverse was calculated, this is retrieved from cache and it's not 
## needed to recalculate it 

cacheSolve <- function(m, ...) {
      inv_ <- m$getinv()
      if (!is.null(inv_)) {
            message("getting cached data")
            return(inv_)
      }
      matrix <- m$get()
      inv_ <- solve(matrix, ...)
      m$setinv(inv_)
      inv_
}
