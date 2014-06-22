## These two functions cache the inverse of a matrix so that the same
## results are not computed repeatedly
## Example: 
# > m <- matrix(c(2,0,0,2),2,2)
# > m
#       [,1] [,2]
# [1,]    2    0
# [2,]    0    2
# > z <- makeCacheMatrix(m)
# > cacheSolve(z)
#       [,1] [,2]
# [1,]  0.5  0.0
# [2,]  0.0  0.5
# > cacheSolve(z)
# getting cached data
#       [,1] [,2]
# [1,]  0.5  0.0
# [2,]  0.0  0.5
# > z$set(matrix(c(3,0,0,3),2,2))
# > cacheSolve(z)
#       [,1]      [,2]
# [1,] 0.3333333 0.0000000
# [2,] 0.0000000 0.3333333


## This function creates a special "matrix" object 
## that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) m <<- inverse
    getinverse <- function() m
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## This function computes the inverse of the special "matrix" returned by 
## makeCacheMatrix above.  If the inverse has already been calculated, then 
## cacheSolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    m <- x$getinverse()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data)
    x$setinverse(m)
    m
}
