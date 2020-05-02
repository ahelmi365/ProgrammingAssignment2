
## These functions introduce a special matrix object which caches its inverse.


#' Create a special "matrix" object that can cache its inverse.
#' 
#' @param x A matrix
#' 
#' @return A list containing four functions to set and get the value of the
#'     matrix and to set and get the inverse of the matrix
#


makeCacheMatrix <- function(x = matrix()) {
        cashe <- NULL
        # Define function to set the value of the matrix.
        #It also clears the old inverse from the cache
        set <- function(y) {
                x <<- y    # Set the value
                cashe <<- NULL # Clear the cache
        }
        # Define function to get the value of the matrix
        get <- function()
                x
        # Define function to set the inverse. This is only used by getinverse() when
        # there is no cached inverse
        setInverse <- function(inverse)
                cashe <<- inverse
        # Define function to get the inverse
        getInverse <- function()
                cashe
        
        # Return a list with the above four functions
        list(
                set = set,
                get = get,
                setInverse = setInverse,
                getInverse = getInverse
        )
}


## Write a short comment describing this function

#' Return inverse of matrix x
#' 
#' This function computes the inverse of the special "matrix" returned by 
#' makeCacheMatrix above. If the inverse has already been calculated 
#' (and the matrix has not changed), then the cachesolve retrieves the 
#' inverse from the cache.
#' 
#' @param x a special matrix created with makeCacheMatrix
#' 
#' @return The inverse of the matrix x
#'
#'
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        cashe <- x$getInverse() # This fetches the cached value for the inverse
        if (!is.null(cashe)) {  # If the cache was not empty, we can just return it
                message("Now getting cached data")
                return(cashe)
        }
        # The cache was empty. We need to calculate it, cache it, and then return it.
        data <- x$get()  # Get value of matrix
        cashe <- solve(data) # Calculate inverse
        x$setInverse(cashe)  # Cache the result
        cashe                # Return the inverse cashed matrix
}
