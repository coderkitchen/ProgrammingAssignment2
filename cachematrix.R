## Functions to cache the inverse of a matrix after it is first generated
## The cache matrix is implemented as a list providing functions to get and
## set both the original matrix and the inverse

## Create a wrapper object for a matrix that allows 
## caching the inverse of the matrix.
makeCacheMatrix <- function(x = matrix()) {
	inverse <- NULL
	#reset cache if original matrix is updated
	setmatrix <- function(y) {
		x <<- y
		inverse <<- NULL
	}
	getmatrix <- function() x
	setinverse <- function(inversematrix) inverse <<- inversematrix
	getinverse <- function() inverse
	list(setmatrix = setmatrix,
		getmatrix = getmatrix, 
		setinverse = setinverse, 
		getinverse = getinverse)
}

## Generate the inverse of a matrix if it has not 
## already been cached and store it for future access
cacheSolve <- function(x, ...) {
	# if inverse is already set on matrix wrapper, return it
	inverse <- x$getinverse()
	if (!is.null(inverse)) {
		return(inverse)
	}
	#generate inverse of original matrix
	inverse <- solve(x$getmatrix(), ...)
	# cache inverse on matrix wrapper and return it
	x$setinverse(inverse)
	inverse
}