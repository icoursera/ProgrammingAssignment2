## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = number()) {
    
    ##Empty variable in function closure
    m <- NULL
    
    ##Define 'set' function to set 'x' & empty 'm'cache variables
    ## Function set variable both in function closure or/and parent environment
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    
    ##return 'x' of function closure
    get <- function() x
    
    ##setSolve function sets or redefines the exiting cached value in function closure or/and parent environment
    setSolve <- function(matrixSolve) m <<- matrixSolve
    
    ##return cached value
    getSolve <- function() m
    
    ##return list of functions
    list(set = set, get = get,
    setSolve = setSolve,
    getSolve = getSolve)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    m <- x$getSolve()
    
    ##check if variable is chached
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    
    ##get non-inverted matrix init in by makeCacheMatrix() func call
    data <- x$get()
    
    ##invert matrix and store in function closure
    m <- solve(data)

    ##populat cache in case it's empty in makeCacheMatrix env.
    x$setSolve(m)
    
    ## return inversed matrix
    m
}
