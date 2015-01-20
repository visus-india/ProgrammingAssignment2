## This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
m <- NULL
## set the value of the matrix
set <- function(y)
{
x <<- y   ## assigning a value to an object in an environment that is different from the current environment
m <<- NULL ## assigning a value to an object in an environment that is different from the current environment
}
get <- function() x  ## get the value of the matrix
setinverse <- function(inverse) m <<- inverse  ## set the inverse of the matrix
getinverse <- function() m    ## get the inverse of the matrix
list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}

## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not changed), then the cachesolve
## should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
## Return the inverse matrix
m <- x$getinverse()
## Check if already cached and skip computation
if(!is.null(m)) {
message("getting cached data")
return(m)
}
data <- x$get() ## if inverse not calculated already get the matrix
m <- solve(data, ...) ## Calculate the inverse
x$setinverse(m) ## Cache the result
m
}

## The above code can be verified by running the below code
verifyresult<-makeCacheMatrix()
verifyresult$set(matrix(1:4,2,2))
cacheSolve(verifyresult)
