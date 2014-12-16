## The following funcitons can compute inverse matrix of invertible matrix.
## Since computing inverse matrix is computationally expensive, the following
## funcitons store inverse matrix computed to reuse it for avoiding computing
## again. Also avoiding error caused by computing empty input, it has default
## value, which is NULL.

## The first function, makeCacheMatrix creates a special "matrix",
## which is really a list containing a function to
## set the matrix
## get the matrix
## set the matrix named inverse
## get the matrix named inverse
## default value of x is NULL
makeCacheMatrix <- function(x = NULL) {
        m <- NULL
        set <- function(y = NULL) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) m <<- inverse
        getinverse <- function() m
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}

## The following function calculates the inverse of the special invertible
## "matrix" created with the above function. 
## However, it first checks to see if the inverse matrix has already 
## been calculated. If so, it gets the inverse matrix from the cache and
## skips the computation. Otherwise, it calculates the inverse of the matrix
## and sets the inverse matrix in the cache via the setinverse function.
## since default value of x is NULL in makeCacheMatrix (), cacheSolve can detect
## matrix is set by set() or not. if matrix is not set yet, it output message.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        if(is.null(data)) {
                message("please set invertible matrix")
                return(data)
        }
        m <- solve(data, ...)
        x$setinverse(m)
        m

}

## example usage.
## xxx <- makeCacheMatrix ()
## cacheSolve(xxx)
## you can get message

## xx <- matrix(c(1,0,0,1),2,2)
## xxx$set(xx)
## cacheSolve(xxx)
## function computes inverse matrix of xxx
## cacheSolve(xxx)
## function read from cache







