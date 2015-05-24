makeCacheMatrix <- function(x=matrix()) {
	cache <- NULL
	setMatrix <- function(y) {
		x <<- y
		cache <- NULL
	}
	getMatrix <- function()x

	setInverse <- function(inverse){
		cache <<- inverse
	}
	getInverse <- function()cache

	list(setMatrix = setMatrix,
	     getMatrix = getMatrix,
	     setInverse = setInverse,
	     getInverse = getInverse)
}

cacheSolve <- function(x,...){
	inverse <- x$getInverse()
	if(!is.null(inverse)){
		message("getting cached data")
		return(inverse)
	}
	data <- x$getMatrix()
	inverse <- solve(data,...)
	x$setInverse(inverse)
	return(inverse)
}