## The pair of functions caches the inverse of a matrix 
## which is useful to avoid repetitive and costly computations.

## makeCacheMatrix creates a special "vector" whose component is functions:
## setting the value of the matrix, getting the value of the matrix, 
## setting the value of the inverse, and getting the value of the inverse.
makeCacheMatrix <- function(data = matrix()) {
        cached.inverse <- NULL
        set <- function(new.data) {
                data <<- new.data
                # if data were changed, forget previously computed inverse
                # since it is not valid anymore
                cached.inverse <<- NULL
        }
        get <- function() {
                return (data)
        }
        setinverse <- function(inverse) {
                cached.inverse <<- inverse
        }
        getinverse <- function() {
                return (cached.inverse)
        }
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## cacheSolve calculates the inverse of the special "vector" created 
## with makeCacheMatrix. If there is the cached inverse, it skips computations 
## and gets the cache. Otherwise, it computes the inverse and sets the value of 
## the inverse in the cache. 

cacheSolve <- function(x) {
        ## Return a matrix that is the inverse of 'x'
        inverse <- x$getinverse()
        if(is.null(inverse)) {
                data <- x$get()
                inverse <- solve(data)
                x$setinverse(inverse)
                return (inverse)
        }
        else {
                message("getting cached data")
                return(inverse)
        }        
}

