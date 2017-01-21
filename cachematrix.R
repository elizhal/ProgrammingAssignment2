## The following two functions compute and cache the inverse of a matrix, which can speed up computations.

## makeCacheMatrix, the first function, creates as special cached matrix by
## 1. setting the matrix
## 2. getting the matrix
## 3. setting the inverse
## 4. getting the inverse

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinv <- function(inverse) inv <<- inverse
    getinv <- function() inv
    list(set=set, get=get, 
         setinv=setinv, 
         getinv=getinv)
}


## Input for "casheSolve" uses the list created in makeCacheMartrix and returns the inverse.
## If the inverse has already been calculated, this function takes it from the cache and skips the computation
## If the inverse has not already been calculated, cacheSolve calculates it

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    inv <- x$getinv()
    
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    mat <- x$get()
    inv <- solve(mat, ...)
    x$setinv(inv)
    
    inv
}

