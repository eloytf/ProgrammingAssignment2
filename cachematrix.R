##
## The following functions help caching the inverse of a matrix, to save computation time
##

## makeCacheMatrix is a helper function that returns a list of functions to be used in cacheSolve

makeCacheMatrix <- function(x = matrix()) {
	inv<- NULL
	set<-function(y) {
		x<<-y
		inv<<-NULL
	}
	get<-function() x
	setinv<-function(y) inv<<-y
	getinv<-function() inv
	list(set=set,get=get,setinv=setinv,getinv=getinv)
}


## cacheSolve uses the previous function to compute the inverse of a matrix, only if it is not cached.
## returns inverse of a matrix
## Example of use: cacheSolve(makeCacheMatrix(a)), where a is a non-singular matrix.


cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
	inv<-x$getinv()
	if(!is.null(inv)){
		message("getting cached data")
		return(inv)

	}
	data<-x$get()
	inv<-solve(data)
	inv
	
}
	