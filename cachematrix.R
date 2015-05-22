## 1. function makeCacheMatrix() creates a special "matrix" object that can cache
##    its inverse
## 2. function cacheSolve() computes the inverse of the special "matrix" returned
##    by makeCacheMatrix above. If the inverse has already been cached, it will 
##    retrieve the result from the cache.


## 1. This fucntion creates a special 'matrix', which is actually a list containing
##    four functions -- set, get, setinverse, and getinverse. 
## 2. You can create a matrix with function set(), get its value with function get(),
##    cache its inverse matrix with function setinverse(), and retrieve this 
##    inverse matrix with function getinverse()
makeCacheMatrix <- function(x = matrix()) {
    inverse <- NULL
    get <- function() x
    set <- function(y) {
        x <<- y #substitute the matrix x in the main function(makeVector)
        inverse <<- NULL
    }
    getinverse <- function() inverse
    setinverse <- function(inv) inverse <<- inv  
    list(set = set, get = get, 
         setinverse = setinverse, 
         getinverse = getinverse)
}


## This function calculates the inverse matrix of the special "matrix" created
## above. It first checks to see if the inverse matrix has already been cached.
## If so, it gets the inverse matrix from the cache. Otherwise, it calculates it
## with function solve(), sets it in the cache, and return its value.
cacheSolve <- function(x, ...) {
    inverse <- x$getinverse()
    if(!is.null(inverse)) {
        message("getting cache inverse matrix")
        return(inverse)
    }
    data <- x$get()
    inverse <- solve(data)
    x$setinverse(inverse)
    inverse
}
