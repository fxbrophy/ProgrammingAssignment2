## 
## R Programming, Programming Assignemnt # 2
##
## script: cacheMatrix.R
## functions defined:
##      1. makeCacheMatrix(): 
##          This function creates a special "matrix" object that can cache 
##          its inverse.
##      2. cacheSolve(): 
##          This function computes the inverse of the special "matrix" 
##          returned by makeCacheMatrix above.  If the inverse has already
##          been calculated (and the matrix has not changed), then
##          cacheSolve() should retrieve the inverse from the cache.
##  Assumptions:
##      1. The matrix passed to makeCacheMatrix() is an invertible matrix.
##      2. The set_inv_mtrx() method defined in makeCacheMatrix() is invoked
##          only by the cachesolve() function.  
## 
## These functions are modeled after the makeVector() and cachemean() 
## functions described in Programming Assignment #2.
##
## usage of these functions is shown in the test execution output
## listed at the bottom of this script.


## makeCacheMatrix()
## description:
##      Create a "CacheMatrix" object that can save an inverse of the
##      matrix passed, to prevent this value from having to be re-computed
##      when needed.
## args:
##      x: a matrix that is invertible
## returns:
##      The list of functions/methods used to set/get the original matrix
##      passed and the cached inverse matrix computed via cacheSolve().
##
##############################################################################

makeCacheMatrix <- function(x = matrix()) {

    # initialize the cached inverse matrix to NULL, to indicate it
    # has not been computed yet
    inverse_x <- NULL
    
    # define 4 functions to:
    #   1.  set/save the value for an invertible matrix passed, and 
    #       init the cached inverse matrix to NULL.
    #   2.  retrieve the value of the invertible matrix that was either
    #       passed to this function or reassigned via set()
    #   3.  set/save the inverse matrix value
    #   4.  retrieve the value of the cached inverse matrix
    set <- function(y) {
        x <<- y
        inverse_x <<- NULL
    }
    
    get <- function() x
    
    set_inv_mtrx <- function(inverse_mtrx) inverse_x <<- inverse_mtrx
    
    get_inv_mtrx <- function() inverse_x
    
    # return value is a list of the 4 functions defined above
    list(set = set, get = get,
         set_inv_mtrx = set_inv_mtrx,
         get_inv_mtrx = get_inv_mtrx)
    
} # end makeCacheMatrix()


##
## cacheSolve()
## description:
##      Return the inverse matrix of the matrix object passed. The matrix 
##      passed must be an object that was created via makeCacheMatrix().
##      If the inverse matrix had already been computed and cached, the
##      cached value is returned.  Otherwise, the inverse matrix value is 
##      computed, cached, and then returned.
## args:
##      x: an object previously created via makeCacheMatrix()
## returns:
##      The inverse of the matrix object for which this function was invoked.
##############################################################################
cacheSolve <- function(x, ...) {
    
    # retrieve the cached inverse matrix of the object passed
    
    inv_mtrx <- x$get_inv_mtrx()
    
    # if the inverse matrix returned was not NULL (was previously computed
    # and cached), return the value retrieved.  Issue a message to indicate
    # the cached value is being retrieved (shows the inverse matrix was not 
    # re-computed).
    
    if(!is.null(inv_mtrx)) {
        message("getting cached data")
        return(inv_mtrx)
    }
    
    # there was no inverse matrix cached for the object passed, so compute
    # the inverse matrix, save it, and return inverse matrix computed.

    data <- x$get()
    inv_mtrx <- solve(data)
    x$set_inv_mtrx(inv_mtrx)
    
    # return inverse matrix of the object passed 
    inv_mtrx
    
} # end cacheSolve()

##############################################################################
##
## test execution output
##
## > source('C:/Users/User1/ProgrammingAssignment2/cachematrix.R')
## > #
## > # first test case: 
## > # 1. define and then pass a 2x2 matrix to makeCacheMatrix()
## > # 2. second exec of cacheSolve() w/this object shows the 
## > #    message indicating the cached inverse matrix was retrieved.
## > m1<-matrix(1:4,2,2)
## > CM1<-makeCacheMatrix(m1)
## > CM1$get()
##      [,1] [,2]
## [1,]    1    3
## [2,]    2    4
## > CM1$get_inv_mtrx()
## NULL
## > cacheSolve(CM1)
##      [,1] [,2]
## [1,]   -2  1.5
## [2,]    1 -0.5
## > cacheSolve(CM1)	# message issued - cached value retrieved
## getting cached data
##      [,1] [,2]
## [1,]   -2  1.5
## [2,]    1 -0.5
## > CM1$get_inv_mtrx() # get inverse matrix directly
##      [,1] [,2]
## [1,]   -2  1.5
## [2,]    1 -0.5
## > #
## > # second test case:
## > # 1. pass a 3x3 matrix to makeCacheMatrix() instead of using a variable
## > # 2. same command sequence as for CM1 above
## > CM2<-makeCacheMatrix( matrix( c( 1:8, 0 ), 3, 3 ))
## > CM2$get()
##      [,1] [,2] [,3]
## [1,]    1    4    7
## [2,]    2    5    8
## [3,]    3    6    0
## > CM2$get_inv_mtrx()
## NULL
## > cacheSolve(CM2)
##            [,1]       [,2]       [,3]
## [1,] -1.7777778  1.5555556 -0.1111111
## [2,]  0.8888889 -0.7777778  0.2222222
## [3,] -0.1111111  0.2222222 -0.1111111
## > cacheSolve(CM2)	# message issued - cached value retrieved
## getting cached data
##            [,1]       [,2]       [,3]
## [1,] -1.7777778  1.5555556 -0.1111111
## [2,]  0.8888889 -0.7777778  0.2222222
## [3,] -0.1111111  0.2222222 -0.1111111
## > CM2$get_inv_mtrx() # get inverse matrix directly
##            [,1]       [,2]       [,3]
## [1,] -1.7777778  1.5555556 -0.1111111
## [2,]  0.8888889 -0.7777778  0.2222222
## [3,] -0.1111111  0.2222222 -0.1111111
## > #
## > # third test case:
## > # exec cacheSolve() for CM1 again to verify the cached value 
## > # is still retrieved after CM2 processing
## > cacheSolve(CM1)
## getting cached data
##      [,1] [,2]
## [1,]   -2  1.5
## [2,]    1 -0.5
## > 
