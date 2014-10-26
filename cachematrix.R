## These two functions cache the inverse of a matrix.

## The following function makes a special matrix which caches the input matrix and it's inverse.
## For this operation, it generates and uses a list of sub-functions
## (called set, get, setinverse, getinverse), which can be used outside of the makeCacheMatrix function.

makeCacheMatrix <- function(x=matrix()) {
	m<-NULL
	set<-function(y) {
		x<<-y
		m<<-NULL
	}
	get<-function() x
	setinverse<-function(solve) m<<-solve
	getinverse<-function() m
	list(set=set, get=get,
		setinverse=setinverse,	
		getinverse=getinverse)
}



## The following function first tests whether the input matrix's inverse
## was counted and cached by the makeCacheMatrix function before.
## If it wasn't, it works as makeCacheMatrix (it counts and caches the new input matrix
## and it's inverse) by using it's sub-functions and solve() function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
m<-x$getinverse()
	if(!is.null(m)) {
		message("getting cached data")
		return(m)
	}
	data<-x$get()
	m<-solve(data, ...)
	x$setinverse(m)
	m
}
