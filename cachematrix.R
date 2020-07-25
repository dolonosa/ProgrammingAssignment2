

# this function takes a numeric matrix and stores it in the cache environment
makeCacheMatrix <- function(x = numeric()) {
    
    # set i variable in local environment
    i <- NULL
    
    # define set function
    set <- function(y) {
        # set matrix x to matrix y in cache environment
        x <<- y
        # set i variable in cache
        i <<- NULL 
    }
    # define get function to store your matrix
    get <- function() x
    
    # define setinverse function to assign it's argument to i in cache environment
    setinverse <- function(inverse) i <<- inverse
    # define getinverse function to store the variable i
    getinverse <- function() i
    
    # create a list to store all four functions
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


# this function retrieves the inverse of a matrix if it is already stored in cache environment
# otherwise it calculates a the inverse and then stores it in the cache environment
cacheSolve <- function(x, ...) {
    # retrieve i variable from cache environment
    i <- x$getinverse()
    # check if i has a value. if it does, print message and  return value of i from cache
    if(!is.null(i)) {
        message("getting cached data")
        return(i)
    }
    # get the matrix from cache
    data <- x$get()
    # calculate the inverse of your matrix
    i <- solve(data, ...)
    # store the value of the inverse in the cache
    x$setinverse(i)
    
    # print the the inverse of the matrix
    i
}