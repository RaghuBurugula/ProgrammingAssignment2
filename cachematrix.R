## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

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


## Write a short comment describing this function

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
