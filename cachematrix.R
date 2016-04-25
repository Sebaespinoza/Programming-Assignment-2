## First, should see README.md 

## This file will include two functions:
## 1. makeCacheMatrix()
## 2. cacheSolve()

## The function makeCacheMatrix creates a "matrix" which also can cache its inverse.
## This function contains the following functions:

# set          set the value of a matrix
# get          get the value of a matrix
# setinver     set the cached value from the inverse of the matrix.
# getinver     get the cached value from the inverse of the matrix.
makeCacheMatrix <- function(x = matrix()) {
        inver <- NULL
        set <- function(w) {
                x <<- w
                inver <<- NULL
        }
        get <- function() x
        setinver <- function(inver_w) inver <<- inver_w
        getinver <- function() inver
        list(set = set, 
        get = get,
        setinver = setinver,
        getinver = getinver)
}


## With the cacheSolve function will be able to calculate the inverse of matrix created by makeCacheMatrix.
## If the inverse matrix has already been calculated and the matrix didn't change, you must to retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
      ##
        inver <- x$getinver()
        
        if (!is.null(inver)) {
                message("getting cached data")
                return(inver)
        }
        
        mem <- x$get()
        inver <- solve(mem, ...)
        x$setinver(inver)
        inver
}
