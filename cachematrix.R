## These fuctions creates a matrix object that can cache its inverse, 
## than computes the inverse and return the value. If the inverse
## value is already in cache it return the cached value.

## Setting and getting the value of the matrix

makeCacheMatrix <- function(x = matrix()) {
	## sets the value of i, inverse, as NULL
	i <- NULL
	## sets the value of the matrix
	set <- function(y) {
		x <<- y
		i <<- NULL
	}
	## gets the value of the matrix
	get <- function() x
	## sets the value of the inverse
	setmatrix <- function(solve) i <<- solve
	## gets the value of the inverse
	getmatrix <- function() i
	## creates a list with the functions
	list(set = set, get = get,
		setmatrix = setmatrix,
		getmatrix = getmatrix)
}

## Calculating the inverse if its not cached

cacheSolve <- function(x = matrix(), ...) {
	## verifying if inverse is already cached
	i <- x$getmatrix()
	if(!is.null(i)){
		## if inverse is cached, returs it
		message("getting cached data")
		return(i)
	}
	## if inverse is not cached, calculate it
	matrix <- x$get ()
	i <- solve(matrix, ...)
	x$setmatrix(i)
	i
}
