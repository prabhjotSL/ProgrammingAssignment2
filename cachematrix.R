# Couple of functions that cache the inverse of a matrix.

# For creating a matrix object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
	# Initializing the inverse property
	i <- NULL

	# Set the matrix
	set <- function( matrix ) {
		m <<- matrix
		i <<- NULL
	}

	# Get the matrix
	get <- function() {
		m
	}

	# Set the inverse of the matrix
	setInverse <- function(inverse) {
		i <<- inverse
	}

	# Get the inverse of the matrix
	getInverse <- function() {
		i
	}

	# Returns list of methods
	list(set = set, get = get,
	   setInverse = setInverse,
	   getInverse = getInverse)
}


# Calculate the inverse of the matrix returned by "makeCacheMatrix". 
# If the inverse has already been calculated (and the matrix has not changed)
# then this method will return the inverse from the cache.

cacheSolve <- function(x, ...) {
	# Return a matrix that is the inverse of 'x'
	m <- x$getInverse()

	# Return the inverse if its value is not NULL
	if( !is.null(m) ) {
		message("getting cached data")
		return(m)
	}

	# Get the matrix
	data <- x$get()

	# Calculate the inverse
	m <- solve(data)

	# Set the inverse
	x$setInverse(m)

	# Return the matrix
	m
}
