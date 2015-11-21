## The following two functions allow the inverse of a matrix to be cached
## This avoids having to compute it repeatedly

## This function creates a "matrix" object which is a list containing a function to
## get the value of the matrix,  set the value of the matrix
## get the value of the inverse,  set the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) m <<- solve
        getinverse <- function() m
        list(set=set, get=get,
             setinverse=setinverse,
             getinverse=getinverse)
        
}


## This function computes the inverse of the "matrix" returned by makeCacheMatrix 
## If the inverse has already been calculated then it is returned from the cache.
## If not, it is calculated and set in the cache

cacheSolve <- function(x, ...) {
        m <- x$getinverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        matrix <- x$get()
        m <- solve(matrix, ...)
        x$setinverse(m)
        m
}
