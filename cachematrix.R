
makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        #set is a function that changes the vector stored in the main function
        get <- function() x
        #get is a function that returns the vector x stored in the main function
        
        setinverse <- function(solve) m <<- solve
        getinverse <- function() m
        # setinverse and getinverse are functions very similar to set and get
        # They do not calculate the mean, they simply store the value of the input in a variable m.
        # into the main function makeVector (setinverse) and return it (getinverse).
        
        list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
        
}


# Function cacheSolve computes the inverse of the special matrix returned by makeCacheMatrix
# If the inverse has already been calculated (and the matrix has not changed),
# then the cachesolve should retrieve the inverse from the cache.
# If the inverse has not been calculated,
# data gets the matrix stored with makeCacheMatrix, m calculates the inverse,
# and x$setinverse(m) stores it in the object m in makeCacheMatrix.

cacheSolve <- function(x, ...) {
        m <- x$getinverse()
        
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m
}
