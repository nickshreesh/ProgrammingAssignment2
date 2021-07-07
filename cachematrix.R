## Put comments here that give an overall description of what your
## functions do

# My comment:
# Instead of repeatedly computing the inversion of a matrix, which can usually
# be costly, there is some advantage to caching the inverse of a matrix. 
# This will be done with the following function: makeCacheMatrix() and 
# cacheSolve(). 



## Write a short comment describing this function

# My comment:
# The function below stores a matrix and caches its inverse. It creates a 
# special object (special matrix) that is a list of: 1) set the value of the 
# matrix, 2) get the value of the matrix, 3) set the value of the inverse, 
# 4) get the value of the inverse. 

# My function:
makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    
    # 1) set the value of the matrix
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    # 2) get the value of the matrix
    get <- function() x
    
    # 3) set the value of the inverse
    setInverse <- function(inverse) inv <<- inverse 
    
    # 4) get the value of the inverse
    getInverse <- function() inv
    
    # Return a list of the above
    list(set = set, 
         get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}



## Write a short comment describing this function

# My comment:
# The function below computes the inverse matrix of the matrix returned by the
# makeCachMatrix() function above. Have the inverse already been calculated, 
# a message "getting cached data" and the inverse matrix will be returned.

# My function:
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    inv <- x$getInverse()
    
    # Returns the inverse and message if its already set
    if (!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    data <- x$get()             # get the matrix
    inv <- solve(data, ...)     # computing the inverse matrix
    x$setInverse(inv)           # set the inverse
    inv                         # return inverse
}
