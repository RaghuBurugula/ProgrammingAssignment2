## The two functions below are created for the ProgrammingAssignment2 in Coursera

## makeCacheMatrix - this function creates a 'matrix' list object containing the 
## setter and getter functions for an input matrix object


makeCacheMatrix <- function(x = matrix()) {
        cachmat <- NULL
        
        setmat <- function(ymat = matrix()) {
                x <<- ymat
                cachmat <<- NULL
        }
        
        getmat <- function() x
        setmatinv <- function(matinv) cachmat <<- matinv
        getmatinv <- function() cachmat
        
        list(set = setmat, get = getmat,
             setinv = setmatinv,
             getinv = getmatinv)
        
}


## cacheSolve - this function tries to return the inverse of the input matrix, first by searching 
## in the cache files and then (if not found in cache) calculates the inverse of the input matrix 
## using the solve function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinv()
        if(!is.null(inv)) {
                message('Retrieving matrix inverse from Cache')
                inv
        }
        
        indata <- x$get()
        invmat <- solve(indata)
        x$setinv(invmat)
        invmat
}
