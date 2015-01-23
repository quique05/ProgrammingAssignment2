## These functions let you to calculate the inverse of a matrix,
## caching it to save time if you need calculate several times 
## the inverse of the same matrix

## This function provides some methods to store and retrieve a matrix and its inverse

makeCacheMatrix <- function(x = matrix()) {
  
        inverseMatrix <- NULL    ## Initial value for inverseMatrix
        
        set <- function(y) {
                x <<- y                 ## Saves the new matrix
                inverseMatrix <<- NULL  ## As the matrix has changed 
                                        ## the inverse matrix is not longer useful
        }
        
        get <- function() x      ## Retrieves the stored matrix
        
        setinverse <- function(inverse) {     ## Stores the inverted matrix
                inverseMatrix <<- inverse
        }
  
        getinverse <- function() inverseMatrix  ## Retrieves the inverted matrix
  
        list(set = set, get = get, setinverse = setinverse,
        getinverse = getinverse)   ## List of available functions in makeCacheMatrix
}

## This function returns a matrix that is the inverse of 'x' 

cacheSolve <- function(x, ...) {
        inverse <- x$getinverse()               ## Get the stored inverse
        
        if (!is.null(inverse)) {                ## If there is a cached inverse, 
                message("getting cached data")  ## print a message and
                inverse <- x$getinverse()       ## retrieve it
        } else {                                ## If there's not a cached inverse,
                inverse <- solve(x$get())       ## calculate the new one and
                x$setinverse(inverse)           ## stores it
        }
        inverse
}
