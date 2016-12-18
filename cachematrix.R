## the following functions cache the value of the inverse
## of a matrix

## defines functions to get, set the matrix and its inverse

makeCacheMatrix <- function(x = matrix()) {
	i<-NULL
	set <- function(y){
			x <<- (y)
			i <<- NULL
	}
	get <- function() x
	setInverse <-function(solve) i<<-solve
	getInverse <- function() i
	list(set=set, get=get,
		setInverse=setInverse,
		getInverse=getInverse)
}


## performs the caching by calling appropriate functions from makeCacheMatrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
	i<-x$getInverse()
	if(!is.null(i)){
		message("getting cached inverse")
		return (i)
	}
	data<-x$get()
	i<-solve(data,...)
	x$setInverse(i)
	i
}
