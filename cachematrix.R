## The function makeCacheMatrix creates a "special" matrix object that 
## can cache its inverse.
##
## this function returns a list containing:
##			1. set the matrix
##			2. get the matrix
##			3. set the inverse of the matrix
##			4. get the inverse of the matrix
##
##
## NOTE: this function assumes that the input matrix is square and invertible 
##
##
makeCacheMatrix <- function(x = matrix()) {
	
			inv = NULL
			set = function (y) {
		
			x<<-y
			inv<<-NULL
	
			}
			get = function() x
	
			setinv = function(inverse) inv <<- inverse
			getinv = function() inv
			list(set=set, get=get, setinv=setinv, getinv=getinv)
	
	}


## The function cacheSolve computes the inverse of the "matrix" returned from
## makeCacheMatrix. the inverse is retrieved from the cache if it has been
## already caculcated and the matrix has not changed!!!


cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        
        		inv = x$getinv()
        
        		if (!is.null(inv)){
        	
        		message("getting cache data")
        		return(inv)
        	
        		}
        
        		mat.data = x$get()
        
        		inv = solve(mat.data, ...)
        
        		x$setinv(inv)
        
        		return(inv)
        
        }
