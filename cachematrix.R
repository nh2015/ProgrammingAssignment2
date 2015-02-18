## FUnctions to create a cached copy of the inverse of a
## matrix for repeated use without incurring the cost of matrix
## inversion after the first call
##
## Functions assume matrix provided are invertible (non-singular) and hence 
## do not perform any additional validation to verify that input matrix is
## invertible
##
## To cache the inverse of a matrix 'A', first create the corresponding cached
## matrix object 'A.cached' using 'A.cached <- makeCacheMatrix()' and then 
## initialize it with original matrix as 'A.cached$set(A)'
##
## To obtain the inverse of A when needed, use 'cacheSolve(A.cached)'
##


## Function to create the cached matrix "object" to hold the original matrix, its
## inverse, member functions to initialize original matrix, return original 
## matrix, create the inverse for the first time and to return the cached
## inverse; Returns the cached marix "object" created

makeCacheMatrix <- function(mat = matrix()) {

    mat.inv <- NULL
    set <- function(y) {
        mat <<- y
        mat.inv <<- NULL
    }
    get <- function() mat
    setInv <- function(inv) mat.inv <<- inv
    getInv <- function() mat.inv
    list(set = set, get = get,
         setInv = setInv,
         getInv = getInv)
}


## Function takes a cached matrix "object" along with other parameters for 
## solve() function, returns the cached inverse of the matrix it had already 
## been created else calls the solve function on the original matrix, 
## returns the newly generated inverse after caching it for future use


cacheSolve <- function(mat, ...) {
        ## Return a matrix that is the inverse of 'mat'
    
    mat.inv <- mat$getInv()
    if(!is.null(mat.inv)) {
        message("Getting cached inverse")
        return(mat.inv)
    }
    message("Computing inverse for the first time")
    mat.orig <- mat$get()
    mat.inv <- solve(mat.orig, ...)
    mat$setInv(mat.inv)
    mat.inv
}
