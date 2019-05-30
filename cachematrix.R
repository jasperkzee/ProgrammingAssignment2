## Two functions to create a "matrix" that can store it's inverse in a cache. Calculates the inverse if no cache for a matrix
## or if the inverse has already been calculated before it will retrieve the cache.
## 

## Returns a list of how to set a matrix, get the matrix, set the inverse, and get the inverse.

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) inv <<- inverse
        getinverse <- function() inv
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}
}


## Returns the inverse of the matrix made in MakeCacheMatrix

cacheSolve <- function(x, ...) {
                inv <- x$getinverse()
                #if the matrix has already been inversed once return the cached, inversed matrix
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        #or else solve for the inverse of the matrix
        data <- x$get()
        inv <- solve(data, ...)
        x$setinverse(inv)
        inv
}   ## Return a matrix that is the inverse of 'x'
}