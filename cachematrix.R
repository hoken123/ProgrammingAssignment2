## These functions calculate and cache the inverse of a matrix.
## The cached inverse can be retrieved if needed

## The function takes a matrix and calculates its inverse.
## The calculated inverse is stored in a cached object for future retrieval

makeCacheMatrix <- function(x = matrix()) {

	myInverse <- NULL		##initialize the object for the inverse matrix

	set <- function(y){	## the set function is used only if a new matrix is to be used
		x <<- y
		myInverse <<- NULL
	}
	get <- function() x
	setInverse <- function(b) myInverse <<- b
	getInverse <-function() myInverse

	list(set = set, get = get, setInverse = setInverse,
		getInverse = getInverse)
}


## This function retrieves the cached inverse matrix of it exists.
## Otherwise, it calculates the inverse and caches the inverse

cacheSolve <- function(x, ...) {	## x should be the object returned from
						## function makeCacheMatrix
	m <- x$getInverse()	## get the cached inverse
	if(!is.null(m)) {		## cached inverse is valid
		message("getting cached inverse")
		return(m)
	}

## cached inverse ls not valid. Neeed to recalculate the inverse and cache the result.
	myMatrix <- x$get()	##get the matrix for which the inverse is required
	m <- solve(myMatrix)	## calculate the inverse
	x$setInverse(m)		## caches the result
	m
}


	
}
