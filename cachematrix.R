## cachematrix.R is an implementation of 'caching' the inverse
## of a matrix, and returning the 'inverse' from the 'cache'
## when appropriate (if a 'cached' inverse exists for the matrix)


## function 'makeCacheMatrix' defines the get and set functions
## for the input matrix and its inverse 
## -- INPUT:  a 'square' matrix (assumed 'invertible')
## -- OUTPUT: list containing the 'get', 'set' functions
##            for the original matrix and its inverse
makeCacheMatrix <- function (matx = matrix()) {
        
        # initialize/purge variable for inverse
        inv <- NULL
        
        # set, get functions for new matrix
        # 'setMat': sets new matrix in global cache when invoked,
        #           resets 'inverse' in global cache
        # 'getMat': retrieves matrix (if already set) from global cache
        setMat <- function (mat_new) {
                matx <<- mat_new
                inv <<- NULL
        }
        getMat <- function () matx
        
        # set, get functions for inverse matrix
        # 'setInv': sets inverse (if not already set) in global cache
        # 'getInv': retrieves inverse (if already set) from global cache
        setInv <- function (matinv) inv <<- matinv
        getInv <- function () inv
        
        # return list of the get, set function pairs for
        # matrix & inverse matrix
        return (list (setMat = setMat, getMat = getMat,
                setInv = setInv, getInv = getInv))
}

## function 'cacheSolve' returns the 'inverse' of the matrix
## either by calculating afresh or by retrieving from the cache
## - the first time 'cacheSolve is explicitly called on a
## - (assumptions: square, invertible) matrix, the 'inverse'
## - will be computed and set in the global cache for that matrix
## -- INPUT:  'square' matrix (assumed 'invertible') and 'optional' fields
## -- OUTPUT: 'inverse' of the square, invertible matrix
cacheSolve <- function (matx, ...) {
        # attempt to retrieve cached inverse for 'matx' and
        # return 'matinv' (the inverse) if 'not' NULL ('inverse' exists)
        matinv <- matx$getInv ()
        if(!is.null (matinv)) {
                message("getting cached data")
                return (matinv)
        }
        
        # if 'matinv' is NULL (has not yet been set in global cache), then
        # - 1. get 'data' from 'matx' global cache
        # - 2. compute inverse ('matinv') by invoking 'base::solve()'
        # - 3. set the computed inverse into global cache for 'matx'
        data <- matx$getMat()
        matinv <- solve (data)
        matx$setInv (matinv)
        
        # return a matrix that is the inverse of 'matx'
        return (matinv)
}
