## Matrix inversion is usually a costly computation and there may be some benefits
## to caching the inverse of a matrix rather than compute it repeatedly. 
## Following two functions are used to cache inverse of a matrix.

## makeCacheMatrix function creates a special "matrix" object that can cache its inverse.
## Created list will contain a function to:
## set a value of the matrix
## get a value of the matrix
## set a value of the matrix inverse
## get a value of the matrix inverse

makeCacheMatrix <- function(x = matrix()) {
	inv <- NULL
	set <- function(y) {
		x <<- y
		inv <<- NULL
}
get <- function() x
setinv <- function(inverse) inv <<- inverse
getinv <- function() inv
list( set = set, get = get,
      setinv = setinv, getinv = getinv)

}


## cacheSolve function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache.
## This function assumes that the matrix is always invertible.

cacheSolve <- function(x, ...) {
        inv <- x$getinv()
	if (!is.null(inv)){
		message ("Getting cached data.")
		return(inv)
	}
	data <- x$get()
	inv <- solve(data)
	x$setinv(inv)
	inv
}


## Example that I run using previous functions:
## > x = rbind(c(1, 2), c(2, 1))
## > m = makeCacheMatrix(x)
## > m$get()
##       [,1] [,2]
## [1,]    1    2
## [2,]    2    1

## No cash first time:
## > cacheSolve(m)
##        [,1]       [,2]
## [1,] -0.3333333  0.6666667
## [2,]  0.6666667 -0.3333333
     
## Retrieving from cash second time:
## > cacheSolve(m)
## Getting cached data.
##        [,1]       [,2]
## [1,] -0.3333333  0.6666667
## [2,]  0.6666667 -0.3333333
##