## The below two functions can compute and cache the inverse of a matrix, thus helping the user avoid
## time-consuming computations.

## Function makeCacheMatrix() creates a "matrix" object that can cache its reverse. Below a brief
## desciption of its structure:

## makeCacheMatrix <- function(x = matrix()) {
## x: a square invertible matrix
## return: a list containing functions to
## 1. set the matrix
## 2. get the matrix
## 3. set the inverse
## 4. get the inverse
## this list is used as the input to cacheSolve().

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setsolve <- function (solve) m <<- mean
        getsolve <- function() m
        list(set = set, get = get,
             setsolve = setsolve,
             getsolve = getsolve)
}           



## Cachesolve() computes the inverse of the matrix returned by makeCacheMatrix(). If
## the inverse matrix has already been calculated, cachesolve() will retrieve it from the cache
## instead of recalculating it.

cacheSolve <- function(x, ...) {
        m <- x$getsolve()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setsolve(m)
        m
}

