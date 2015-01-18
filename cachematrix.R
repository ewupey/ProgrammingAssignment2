## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

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


## Write a short comment describing this function

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

