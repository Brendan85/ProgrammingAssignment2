## First function is actually just a list of other functions
## It essentially caches the inverse of any given matrix
## Second function uses the cached inverse, checks if it is in fact the inverse, if it is returns the inverse, if not, calculates the inverse and pushes the inverse back to the cached version

## Essentially a list of a few functions that can be called on for caching and calculating the inverse of a matrix

makeCacheMatrix <- function(x = matrix()) {
    invs <- NULL ##invs=inverse
    set <- function(y) { ##Function that stores the input and nulls
        x <<- y
        invs <<- NULL
    }
    get <- function() {x} ##anonymous function, essentially just to return x out of the function
    setinvs <- function(solve) {invs <<- solve}
    getinvs <- function() {invs}
    list(set = set, get = get,
         setinvs = setinvs,
         getinvs = getinvs)
}


## This function first checks if the inverse is cached. If it is it returns it with a message. If not it calculates it and pushes the inverse back into cache for future use.
## Returns a matrix that is the inverse of the inputted matrix

cacheSolve <- function(z, ...){
  inverse<-z$getinvs
  if(!is.null(inverse)){ ##test if inverse is cached
    message("Matrix already cached")
    return(inverse)
  }
  data<-z$get()
  inverse<-solve(data)
  z$setinvs(inverse)
  inverse
}

