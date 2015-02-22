## High-level overview (or problem statement)
## This program implements functions to 1. store a matrix and it's inverse , 2. to compute inverse of a matrix or return cached value if available .
## It benefits from the cached inverse value, assuming the matrix has not changed.
## Functions used are :
## makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
## cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache.
 

 
 
## **** Function makeCacheMatrix ****
## This takes (invertible) matrix as argument x.
## It defines the following functions in it .
##   1. get         - returns matrix         
##   2. getinverse  - returns inverse of the matrix
##   3. set         - To store/cache the matrix value; also resets the inverse to NULL value.
##   4. setinverse  - To store/cache the computed inverse value for the matrix 
## The "set" functions 3 and 4 uses the special operator "<<-"  to set/access values out side of the function
## Return value: above 4 functions are stored in a list  and returned as output of the function; 
##               These returned-functions provide access to the cached values (from other functions).

makeCacheMatrix <- function(dataMatrix = matrix()) {
        resultMatrix <- NULL     #-- will cache "inverse result" for the the input matrix , ie., dataMatrix .
		
        #-- to store new input matrix and reset inverse-result matrix to null		
        set <- function(argMatrix) {
                dataMatrix <<- argMatrix
                resultMatrix <<- NULL
        }
        
		get <- function() {
		    dataMatrix
        }
		
		setinverse <- function(computedInverse) {
		  resultMatrix <<- computedInverse
        }
		
		getinverse <- function() {
		  resultMatrix
        }
		
		list(set = set, 
		     get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}

## Function cacheSolve :
## Arguments :  "list-of-functions" (as returned by makeCacheMatrix).
## Calls these functions (from the argument list) to get the computed value, ie.,inverse of a matrix .
## If the pre-computed inverse value is available (in the cache), it is returned. 
## If the inverse value is not cached yet, do the following
##    1. get the matrix   (The assumption is , this matrix doesn't change and the cached inverse value is used often) 
##    2. compute inverse (using solve function), 
##    3. cache it        (so the cached value will be available from the next call).
##    4. return inverse matrix

cacheSolve <- function(matrixVector, ...) {
        ## Return a matrix that is the inverse of 'needInverse'
	    inverseMatrix <- matrixVector$getinverse()

        ## verify cached result and return it
		if(!is.null(inverseMatrix)) {
                message("getting cached data")
                return(inverseMatrix)
        }
		## else compute result and cache it by calling functions in matrixVector argument
        dataMatrix <- matrixVector$get()
        inverseMatrix <- solve(dataMatrix)
        matrixVector$setinverse(inverseMatrix)
        inverseMatrix
}
