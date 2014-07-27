## Two functions to speed processing of matrices.

## This function takes a matrix x and adds some function
# handles to interact with the matrix.  

makeCacheMatrix <- function(x = matrix()) {
        MI <- NULL
        set <- function(y) {
                x <<- y
                MI <<- NULL
        }
        get <- function() x
        setinverse <- function(inverted_matrix) MI <<- inverted_matrix
        getinverse <- function() MI
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## This function will solve for the inverse of the matrix and 
# store that value in the workspace, accessible with x$getinverse()

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        MI <- x$getinverse()
        if(!is.null(MI)) {
                message("getting cached data")
                return(MI)
        }
        data <- x$get()
        MI <- solve(data, ...)
        x$setinverse(MI)
        MI
}
