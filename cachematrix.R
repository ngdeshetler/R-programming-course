## These two functions, makeCacheMatrix and cacheSolve, can be used to cache the inverse
## of a matrix.  This can be used to reduce the time required when an matrix's inverse is
## required on several occations


## This function creates a matrix-like object, in the form of a list,
## that can be used to cache that matrix's inverse.
##
## This function returns a list of four functions that allow the martix x and its inverse
## to be retreived or set

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL # When creating this matrix-list the inverse has not yet been calculated
    set <- function(y){
        ## This function allows for the matrix to be replaced. The inverse is reset to null
        x <<- y
        i <<- NULL
    }
    get <- function() x #returns the original matrix x
    setinverse <- function(inver) i <<- inver #caches a calculated inverse of the matrix x
    getinverse <- function() i #returns the inverse of the matrix x, either NULL if not set
    ## or inver previously set by setinverse
    
    list(set=set, get=get, setinverse=setinverse, getinverse=getinverse) #list containing the
    # four functions need to get and set the matrix X and its inverse
}


## This function returns the inverse of a matrix-like list object x (created with makeCacheMatrix).
##
## The inverse is either calculated if this is the first instance of calling cacheSolve on x (this 
## calculated value is cached for later use) or retrieves the cached  inverse previously calculated by cacheSolve.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    i <- x$getinverse() #gets the cached inverse of X
    if(!is.null(i)){ #if inverse has been calculated before
        message("getting cached data")
        return(i) #returns previously calculated inverse
    }
    data <- x$get() #get the original matrix
    i <- solve(x) #calculate inverse
    x$setinverse(i) #cache calculated inverse for later use
    i #returns the inverse of x
}
