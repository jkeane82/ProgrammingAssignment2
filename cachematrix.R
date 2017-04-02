##############################################################################################################################
# - Script provides two functions where the inverse of a matrix is computed (assuming matrices are invertible).              #     
# - Computing inverses of matrices can be costly so these functions provide a mechanism where computed inverses are stored   #
#   in a cache where it can be recalled if the inverse of the matrix is needed again.                                        #
#                                                                                                                            #
# Name: James Keane                                                                                                          #
# Course: R Programming                                                                                                      #
# Week: 3                                                                                                                    #
# Assignment: 2                                                                                                              #
##############################################################################################################################


# This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
     # Initially the matrix x saves the matrix that is passed through the function into a new environment, that is, its own namespace so that the functions within
     # this environment have acces to values that are not part of the global environment.
     
     # Set initial inverse variable to NULL
     inv <- NULL
     
     # Internal function that saves the NEW matrix and initiates its inverse as NULL
     set <- function(y) {
          x <<- y
          inv <<- NULL
     }
     
     # Internal function that retrives the cached matrix
     get <- function() x
     
     # Internal function that calculates the inverse of the cached matrix
     setinverse <- function(solve) inv <<- solve            
     
     # Internal function that retrives the cached inverse
     getinverse <- function() inv
     
     # Creates a list of the internal functions of makeCacheMatrix so that they can individually be called outside this environment
     list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}

# This function computes the inverse of the special"matrix" returned by `makeCacheMatrix` above. If the inverse has already been calculated 
# (and the matrix has not changed), then`cacheSolve` should retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
     
     # Retrive the cached inverse of the matrix
     inv <- x$getinverse()
     
     # Verify that the inverse has already been calculated
     if(!is.null(inv)) {
          message("getting cached data")
          return(inv)
     }
     
     # If the inverse of the matrix has not be evaluated than do so in the following code
     data <- x$get()          # Retrive matrix
     inv <- solve(data)       # Evalute the inverse of the matrix
     x$setinverse(inv)        # Save the inverse of the matrix
     inv                      # Retrun the inverse of the matrix
}
