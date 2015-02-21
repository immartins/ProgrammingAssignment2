## Matrix inversion usually being a costly computation, there
## may be some benefit in caching the inverse of a matrix rather
## than computing it repeatedly.

## Create the list of functions to cache the inverse of a matrix.
makeCacheMatrix <- function(x=matrix()) {
    mx <- NULL
    set <- function(y) {
        x <<- y
        mx <<- NULL
    }
    get <- function() x
    setInv <- function(inv) mx <<-inv
    getInv <- function() mx
    list(set = set, get = get, setInv = setInv, getInv = getInv)
}
 
## Compute the inverse of the matrix returned by the previous function
## if (A) it has not been calculated before. If (A), use cached data.
cacheSolve <- function(x, ...) {
    mx <- x$getInv()
    if ( ! is.null(mx)) {
        print("using cached data")
        return(mx)
    }
    mx <- solve(x$get())
    x$setInv(mx)
    mx
}
 
 
## Test  
q <- makeCacheMatrix(matrix(1:4,2))
q$get()
q$getInv()
q$set(matrix(5:8,2))
q$get()
cacheSolve(q)
cacheSolve(q)
q$getInv()
r = q$getInv()
q$get() %*% r
