## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
        inv = NULL                      ## initialize inv as NULL; This will hold value of matrix as inverse  
        set <- function(y) {            ## define the set function to assign new  
                x <<- y                 ## value of matrix in parent environment 
                inv <<- NULL            ## if there is a new matrix, reset inv to NULL 
        } 
        get <- function() x             ## define the get function - returns value of the matrix argument 
        
        
        setinverse <- function(inverse) inv <<- inverse  ## assigns value of inv in parent environment 
        getinverse <- function() inv                     ## gets the value of inv where called 
        list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)    ## you need this in order to refer  
        ## to the functions with the $ operator 
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinverse() ##Initialize INV as the inverse of X
        if(!is.null(inv)) { ## If Loop on inv to see if it is NOT NULL
                message("getting cached data") ##Message you will see
                return(inv) ## returns the Function of inv (inverse of x)
        } 
        data <- x$get() ## Returns the value of the named object
        inv <- solve(data, ...) ## Solves the equation
        x$setinverse(inv) 
        inv 
}
