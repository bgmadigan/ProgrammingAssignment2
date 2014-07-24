##
## R Programmimg - Assignment 2
## ----------------------------
## 
## Author:          Brendan Madigan
##                  Melbourne, Victoria, Australia
##
## makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
## 
## cacheSolve     : This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
##                  If the inverse has already been calculated (and the matrix has not changed), 
##                  then the cachesolve should retrieve the inverse from the cache.
##



makeCacheMatrix <- function(x = matrix()) {
##
## 'x' is a square maytrix
##
##   1.set the value of the matrix
##   2.get the value of the matrix
##   3.set the value of the inverse of the matrix   
##   4.get the value of the inverse of the matrix

## 
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) m <<- solve
        getinverse <- function() m
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)

        



}

##  B = matrix( c(1:9),   nrow=3, ncol=3)

## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
       m <- x$getinverse()
        if(!is.null(m)) {
                message("getting cached matrix")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m

}
