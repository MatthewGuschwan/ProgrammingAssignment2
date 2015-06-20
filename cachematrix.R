## These functions allow you to create a special matrix and to calculate its inverse while making this calculation available outside of the immediate 
## environment.

## makeCacheMatrix creates a 'fancy' matrix that includes a matrix along with functions to set, and get the inverse and store the inverse in the cache 
## using the <<- superassignment command


makeCacheMatrix <- function(x = matrix()) {   ## creates mCM function - to be passed a matrix
    inv <- NULL                               ##  inverse of matrix default set to NULL (it has not been calculated)
    set <- function(y) {                      ##  set function allows us to pass along a new matrix to replace old one (x)
        x <<- y                                 ## x is now available outside of this environment
        inv <<- NULL                            ## inverse is set to null, because we have a new matrix, so inv needs to be re calculated
    }
    get <- function() x                         ## allows us to get the original matrix (not the souped up, function-ed out one)
    setinverse <- function(solved) inv <<- solved     ##  set inverse is intended to let us set the inverse in the wider environment (though it really just sets inv to whatever we pass it in 'setinv(solved)')
    getinverse <- function()  inv               ## simply returns what is stored in inv (should be the inverse of original matrix)
    list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)    ## creates the list of functions attached to the matrix
}



## cacheSolve returns the inverse of a 'fancy' matrix, and calculates and sets the value of the fancy matrix if necessary.

cacheSolve <- function(x, ...) {    ## Return a matrix that is the inverse of the 'fancy' matrix, 'x' using solve()
      inv <- x$getinv()             ## inv is set to the current value of inv in the x 'function-list' matrix
      
      if(!is.null(inv)) {           ## test to see if there is already a value for inv (if is has already been calculated for this x matrix)
            message("getting cached inverse")    ## if inverse has already been calculated, simply return it with a nice message
            return(inv)
      }
      
      simpleM <- x$get()            ## extracts the Simple Matrix (without all the functions) from the fancy matrix
      inv <- solve(simpleM, ...)    ## finally calculates the inverse of the simple matrix and stores result in inv
      x$setinverse(inv)             ## takes the just calculated inverse, and stores it in the 'fancy' matrix
      inv                           ## returns this just calculated inverse
}