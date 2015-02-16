## Programming Assignment 2 - Swati Rane
## makeCacheMAtrix: creates a special "matrix" object that caches its inverse

## This function creates a special "matrix" object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinv <- function(solve) m <<- solve(x,diag(nrow(x))) 
        getinv <- function() m
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)        

}


## Check if matrix inverse is already cached, if YEs, display it otherwise calculate it

cacheSolve <- function(x, ...) {
        m <- x$getinv()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data,diag(nrow(data)))
        x$setinv(m)
        m
}
