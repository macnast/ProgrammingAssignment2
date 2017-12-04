## The following functions are designed to calculate an inverse of matrices
## Once that is done, the result is stored in cache, to be used if asked
## for it again (and to save time)

## The first function saves the matrix and its inverse in cache

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setInverse <- function(solve) m <<- solve
    getInverse <- function() m
    list(set = set, get = get, setInverse = setInverse,
         getInverse = getInverse)
}

## The second matrix digs into the first one, looking for cached data. 
## If an inverse for a given matrix is stored, it is printed 
## along with a message. Otherwise, the fucntion caculates the inverse

cacheSolve <- function(x, ...) {
    m <- x$getInverse()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setInverse(m)
    m
}
