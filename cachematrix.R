## Computing the inverse of a matrix using memoization.
##
## makeCacheMatrix creates a matrix "object" that can cache its inverse.
## cacheSolve computes the inverse of a matrix, first checking to see if there's 
## a cached result and, if not, computes the matrix using solve() and caches the result.
##
## Use as follows:
## > m <- makeCacheMatrix(matrix(c(0, 1, 4, 1, 0, -3, 2, 3, 8), 3, 3))
## > cacheSolve(m)
## > m$getinverse()

## makeCacheMatrix creates a memoized matrix "object" that can cache the results of solving 
## for the inverse. The "object" created by this fucntion is really a list of four accessor
## functions that set and get the matrix and its inverse. They use R's lexical scoping rules to
## bind variables to the matrix and the inverse of the matrix, and access them.
##
## Arguments: x, a matrix (defaults to an empty matrix). Assume x is invertible.
## Returns: The list of the functions set, get, setinverse, and getinverse

makeCacheMatrix <- function(x = matrix()) {
    x.inv <- NULL
    
    # set binds x, the variable that stores the matrix, to its argument, y and resets the
    # inverse to NULL. It uses "<<-" to search the parent environments and change the
    # definitions of the variables.
    set <- function(y) {      
            x <<- y
            x.inv <<- NULL
    }
    # get returns the matrix stored in x
    get <- function() x
    # setinverse binds x.inv, the inverse of x, to its argument
    setinverse <- function(inverse) x.inv <<- inverse
    # getinverse returns the inverse of x
    getinverse <- function() x.inv
    
    list(set = set, get = get,     # return a list of the functions
         setinverse = setinverse,
         getinverse = getinverse)
}


## cacheSolve computes the inverse of a matrix. The frst time it's called it will cache 
## the inverse, and subsequently will return the cached result.
##
## Arguments: x, a matrix "object" created by makeCacheMatrix
##            ..., arguments to pass to solve()
## Returns: the inverse of the matrix, x

cacheSolve <- function(x, ...) {
    m <- x$getinverse()
    # if the inverse has been cached, return it
    if(!is.null(m)) {
            message("getting cached data")
            return(m)
    }
    # otherwise, get the actual matrix and use solve() to compute the inverse
    data <- x$get()
    m <- solve(data, ...)
    x$setinverse(m)    # cache the result
    m
}
