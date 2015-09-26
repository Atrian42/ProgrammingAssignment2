##
## Function: makeCacheMatrix
## Purpose:  create a special "vector", consisting of a list 
##           containing a function to:
##
## 1. set the value of the vector
## 2. get the value of the vector
## 3. set the value of the inversed matrix
## 4. get the value of the inversed matrix
##
makeCacheMatrix <- function(x = numeric()) {
    i <- NULL
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) i <<- inverse
    getinverse <- function() i
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}

cacheSolve <- function(x, ...) {
    i <- x$getinverse()
    if(!is.null(i)) {
        message("getting cached data.")
        return(i)
    }
    data <- x$get()
    i <- solve(data)
    x$setinverse(i)
    i
}

## Simulated run:
## Create sample matrix
##
## > x = rbind(c(5, -1/8), c(-1/8, 5))
## > m = makeCacheMatrix(x)
## > m$get()
## [,1]   [,2]
## [1,]  5.000 -0.125
## [2,] -0.125  5.000

## no cached data
##
## > cacheSolve(m)
## [,1]        [,2]
## [1,] 0.200125078 0.005003127
## [2,] 0.005003127 0.200125078

## data now cached
##
## > cacheSolve(m)
## getting cached data.
## [,1]        [,2]
## [1,] 0.200125078 0.005003127
## [2,] 0.005003127 0.200125078
## > 