## Caching the inverse of a matrix
##

## sets a matix that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
         invr <- NULL
         set <- function(y) {
                 x <<- y
                 invr <<- NULL
         }
         get <- function() x
         setmatrix <- function(solve) invr <<- solve
         getmatrix <- function() invr
         list(set = set, get = get,
              sematrix = setmatrix,
              getmatrix = getmatrix)
}


## calculates the inverse of the matrix

cacheSolve <- function(x, ...) {
        invr <- x$getmatrix()
        if(!is.null(m)){
                message("getting cached data")
                return(invr)
        }
        ## Return a matrix that is the inverse of 'x'
        data <- x$get()
        invr <- solve(data, ...)
        x$setmatrix(invr)
}
