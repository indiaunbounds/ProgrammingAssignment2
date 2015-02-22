
# Matrix inversion is usually a costly computation and there may be some benefit
# to caching the inverse of a matrix rather than compute it repeatedly. The
# following two functions are used to cache the inverse of a matrix.

# makeCacheMatrix creates a list containing a function to
# set and get the value of the matrix and of inverse of the matrix
makeCacheMatrix <- function(x=matrix()) {
## Creates a list of functions that
## can cache the inverse of a matrix.
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setInverse <- function(inverse) m <<-inverse
    getInverse <- function() m
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)

}

cacheSolve <- function(x, ...) {
## Computes the inverse of the matrix returned by makeCacheMatrix(), unless the inverse has
## already been calculated, in which case it retrieves it from the cache.
    m <- x$getInverse()
    if ( ! is.null(m)) {
        print("fetching cached data")
        return(m)
    }
    m <- solve(x$get())
    x$setInverse(m)
    m
}
## Below is a Sample output
## > a <- makeCacheMatrix(matrix(1:4,2)) 
# > a$get()
     # [,1] [,2]
# [1,]    1    3
# [2,]    2    4
# > a$getInverse()
# NULL
# > a$set(matrix(5:8,2))
# > a$get()
     # [,1] [,2]
# [1,]    5    7
# [2,]    6    8
# > cacheSolve(a)
     # [,1] [,2]
# [1,]   -4  3.5
# [2,]    3 -2.5
# > cacheSolve(a)
# [1] "fetching cached data"
     # [,1] [,2]
# [1,]   -4  3.5
# [2,]    3 -2.5
# > a$getInverse()
     # [,1] [,2]
# [1,]   -4  3.5
# [2,]    3 -2.5
# > b = a$getInverse()
# > a$get() %*% b
     # [,1]         [,2]
# [1,]    1 3.552714e-15
# [2,]    0 1.000000e+00
# > 
]
