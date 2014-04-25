## This R code creates a special function to cache the inverse of a matrix.
## When solving the inverse of a square matrix the cachesolve function checks if the value exists in cache
## By re-using cached values, performance of the code is faster

## Create a function that lists functions to get and set in cache the inverse of a matrix

makeCacheMatrix <- function(x = matrix()) {
    si <- NULL                # set inverse value to NULL
    set <- function (y) {      # function to set value of square matrix
      x <<- y
      si <<- NULL
    }
    get <- function () x            # get matrix
    setinverse <- function (inverse) {   # function to set inverse matrix value
      si <<- inverse
    }           
    getinverse <- function () si    # get value of inverse matrix
    list (set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## Create a function to set the inverse of a matrix in cache.  Use square matrix

cacheSolve <- function(x, ...) {
        si <- x$getinverse()        ## query the x matrix cache
        if (!is.null(si)) {         ## check if inverse matrix value is in cache
              message ("getting cached data")
              return(si)
        }
        data <- x$get()             
        inverse <- solve(data)      ## if not in cache, calculate inverse
        x$setinverse(inverse)       ## set the value of inverse matrix in cache
}
