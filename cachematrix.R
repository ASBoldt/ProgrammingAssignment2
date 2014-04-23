## Put comments here that give an overall description of what your
## functions do

# The makeCacheMatrix function takes an argument x (matrix),
# and returns a list with 4 items.

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y){
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinv <- function(solve) inv <<- solve
    getinv <- function () inv
    list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## Write a short comment describing this function

# The cacheSolve function works with makeCacheMatrix function.
# It takes the "list's" arguments as inputs and outputs the 
# inverse of a matrix (from makeCacheMatrix).

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    inv <- x$getinv()
    if(!is.null(inv)){
        message("getting cached data")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data,...)
    x$setinv(inv)
    inv
}
