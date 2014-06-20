## Functions that enable the memoized computation of the inverse of matrix
## makeCacheMatrix creates a matrix "object" that can cache the matrix's inverse.
## cacheSolve computes the inverse of a matrix, first checking to see if there's 
## a cached result and, if not, computes the matrix with solve() and caches the result.
##
## Use as follows:
## > m <- makeCacheMatrix(matrix(c(0, 1, 4, 1, 0, -3, 2, 3, 8), 3, 3))
## > cacheSolve(m)
## > m$getinverse()

## makeCacheMatrix creates a memoized matrix "object" that can cache the results of solving 
## for the inverse. This function actually creates an environment with a variable, x.inv, that stores
## the inverse and functions that set and get the matrix and its inverse.
##
## Arguments: x, a matrix (defaults to an empty matrix)
## Returns: A list of the functions set, get, setinverse, and getinverse

makeCacheMatrix <- function(x = matrix()) {
    x.inv <- NULL
    
    set <- function(y) {      # define set, get, setinverse, getinverse
            x <<- y
            x.inv <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) x.inv <<- inverse
    getinverse <- function() x.inv
    
    list(set = set, get = get,     # return a list of the functions
         setinverse = setinverse,
         getinverse = getinverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    m <- x$getinverse()
    if(!is.null(m)) {
            message("getting cached data")
            return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setinverse(m)
    m
}
