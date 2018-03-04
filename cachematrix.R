## Creation of a special "Matrix" by making the function named, "makeCacheMatrix".
## As per the examples given in the instructions, first we create the Matrix, which is essentially a list of:
        ## "set", which is a value of the matrix;
        ## "get", which is a value of the matrix and retrieves the set value;
        ## "set_inverse", which is the value of inverse (Solve) of the matrix;
        ## "get_inverse", which is a value and retrieves the inverse (Solve) of the matrix.

makeCacheMatrix <- function(x = matrix()) {                     ## Creation of the function, "makeCachematrix"
        inv <- NULL                                             ## Effectively sets the "inverse" of object 'm'
        set <- function(y) {                                    ## Creation of the "set" variable, which is a function of the 'y' argument       
                x <<- y                                         ## Assigns a value to the 'x' object within the local environment of 'y'
                inv <<- NULL                                    ## Setting the "inverse" of object "inv" within the "set" function
        }
        get <- function() x                                     ## Creation of the "get" variable
        set_inverse <- function(inverse) inv <<- inverse        ## Creation of the "set_inverse" variable which is a function 'inverse' derived by the setting of, "inv <- NULL"
        get_inverse <- function() inv                           ## Creation of the "get_inverse" variable which is a function derived by calling the "inv"
        list(set = set                                          ## Creation of a list consisting of the four previously created variables^^^^
             , get = get
             , set_inverse = set_inverse
             , get_inverse = get_inverse)
}
        
## The Function below computates the inverse of a matrix "x"
## This matrix "x" will be created via the "makeCacheMatrix" function above. The following function
## will retrieve the inverse from cache IF the inverse has been calculated already.

cacheSolve <- function(x, ...) {                                ## Creation of the function, "cacheSolve" 
        inv <- x$get_inverse()                                  ## The Inverse(Inv) is set through calling "$getinverse" from the matrix "x" above
        if(!is.null(inv)) {                                     ## "if" loop designed to check and see if the inverse has been calculated already--
                message("Getting Cached Data...")               ## --and if it has, will print a message on the screen informing the user the inverse is
                return(inv)                                     ## --is being pulled from the Cached Data.
        }                                                       ## If there is no information in the Cached Data from the "if" loop above--
        mat_data <- x$get()                                     ## "inv" is set through solving the inverse of the new matrix
        inv <- solve(mat_data, ...)                             ## New Matrix Data (mat_data) is set through calling "$get" data from the matrix "x" function above
        x$set_inverse(inv)                                      ## This sets the inverse ($set_inverse) of "inv" for any new matrix thats entered using the "cacheSolve" function
        inv 
          
}
## Example used for testing functions:

## Create an invertable matrix named "matrix1"
matrix1 <- matrix(runif(1e2, 1, 10), 1e1, 1e1)

## Create the matrix utilizing "makeCacheMatrix" function
matrix_final <- makeCacheMatrix(matrix1)

## Solve the Matrix utilizing the Inverse function via "cacheSolve"
cacheSolve(matrix_final)

## Repeat command, to Pull the Matrix from Cache and get the Message on the Screen:
cacheSolve(matrix_final)