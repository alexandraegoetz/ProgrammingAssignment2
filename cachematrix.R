## The makeCacheMatrix function will create a matrix object that can 
## cache it's inverse, so it can be remembered for future changes, 
## and doesn't need to be re-computed each time.

## The cacheSolve function will actually compute the inverse of the matrix
## to be stored using makeCacheMatrix.

## makeCacheMatrix will take an argument of matrix type and
## initialize the inverse.

makeCacheMatrix <- function(x = matrix()) {
	## Make null matrix which will be set to inverse of matrix x
	inverse <- NULL
	## Use set function to set matrix x to matrix argument. Reset
	## inverse to NULL, as indication that the matrix has changed.
      set <- function(y = matrix()) {
      	x <<- y
            inverse <<- NULL
      }
	## Use get function to return matrix stored in x
      get <- function() x
	## Use setInverse function to set inverse of matrix x
      setInverse <- function(i) inverse <<- i
	## Use getInverse function to determine current inverse value
      getInverse <- function() inverse
	## Return a list of outputs for set, get, setInverse and getInverse.
      list(set = set, get = get,
      	setInverse = setInverse,
      	getInverse = getInverse)
}

## Use cacheSolve to compute inverse of matrix x. Takes argument x.

cacheSolve <- function(x, ...) {
	## Set inverse equal to the inverse of matrix 'x'
	inverse <- x$getInverse()
	## If inverse already has been calculated, return that value.
      if(!is.null(inverse)) {
      	message("getting cached matrix")
      	return(inverse)
      }
	## If inverse has not been computed, compute inverse of 'x'.
      data <- x$get()
      inverse <- solve(data, ...)
	## Set computed inverse of 'x' into it's values.
      x$setInverse(inverse)
	## Return a matrix that is the inverse of 'x'
      inverse
}
