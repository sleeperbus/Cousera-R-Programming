###############################################################################
#   Calculate inverse matrix and cache the result for reusablity
###############################################################################


# Initialize orgMat variable to x as soon as this object is created.
makeCacheMatrix <- function(x = matrix()) {
    orgMat <- NULL
    inverseMat <- NULL
    
    # save original matrix
    setMat <- function(mat) {
        orgMat <<- mat
        inverseMat <<- NULL
    }
    # return original matrix
    getMat <- function() orgMat
    # save inverse matrix
    setInverseMat <- function(mat) inverseMat <<- mat
    # return inverse matrix
    getInverseMat <- function() inverseMat
    
    # initialize orgMat var
    setMat(x)
    
    list(setMat = setMat, getMat = getMat, setInverseMat = setInverseMat,
         getInverseMat = getInverseMat)    
}

# return cache data if exists
# otherwise use solve function to calculate inverse matrix and save to
# caller object
cacheSolve <- function(x, ...) {
    inverseMat <- x$getInverseMat()
    if (!is.null(inverseMat)) {
        message("getting from cache")
        return (inverseMat)
    }
    x$setInverseMat(solve(x$getMat()))
    inverseMat <- x$getInverseMat()
    inverseMat
}
