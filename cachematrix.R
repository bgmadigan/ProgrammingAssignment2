##
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
##
makeCacheMatrix <- function(x = matrix()) {
##
##   makeCacheMatrix takes as its input:
##
##      'x' - a square matrix 
##        
##   NOTE: no checks are performed to ensure that x is a square matrix
##
##   1.set the value of the matrix
##   2.get the value of the matrix
##   3.set the value of the inverse of the matrix using "solve"   
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


cacheSolve <- function(x, ...) {
##
##   cacheSolve takes as its input:
##
##      'x' - square matrix
##        
##   NOTE: no checks are performed to ensure that x is a square matrix
##
##   If the inverse matrix has already been determined
##      return the cached matrix (to save calculation time)
##   otherwise
##      use "solve" todetermine the inverse matrix
##      return the matrix
##       
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
