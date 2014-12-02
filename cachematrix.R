## Matrix inversion is usually a costly computation and there may be some benefit to caching the inverse
## of a matrix rather than computing it repeatedly (there are also alternatives to matrix inversion that we will not discuss here).
## 

#  Overview
#1. makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
#2. cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. If the inverse has already
#been calculated (and the matrix has not changed), then the  cachesolve should retrieve the inverse from the cache.

# taken from example makeVector code:

# set the value of the matrix
# get the value of the matrix
# set the inverse of the matrix
# get the inverse of the matrix
makeCacheMatrix <- function(x = matrix()) {
	this_inv <-NULL
	set <-function(y){
		x<<- y
		this_inv<<-NULL
	}#function 
	get <- function()x
	set_inverse <- function(inverse) this_inv <<- inverse
	get_inverse <- function() this_inv
	list(set=set,get=get,set_inverse=set_inverse,get_inverse=get_inverse)
}# makeCacheMatix

# no error checking for inverse of matrix is possible
# interaction with the makeCacheMatrix object above.
# based on the example makeVector code
cacheSolve <- function(x, ...) {
	this_inv<- x$get_inverse()
	if (!is.null(this_inv)){
		message("getting CACHED inverse matrix")
		return(this_inv)
	}#if
	data<-x$get() 
	this_inv <-solve(data,...)
	x$set_inverse(this_inv)
	this_inv
}# cacheSolve
