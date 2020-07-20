## Put comments here that give an overall description of what your
## functions do


## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) { ##create the function
        invert <- NULL ##assume the matrix we are making is invertible
        set <- function(y) {
                x <<- y
                invert <<- NULL #double arrow is able to modify values at the parent level
        }
        get <- function() {x} ##get the matrix
        setInverse <- function(inverse) {invert <<- inverse} ##set the value of the inverse
        getInverse <- function() {invert} ##to get the value of the inverse
        list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}

cacheSolve <- function(x, ...){ 
        invert <- x$getInverse() ##gets the inverse of x
        if(!is.null(invert)){
                message("Retrieving Cached Data") ##checks if already cached
                return(invert)
        }
        mat <- x$get()
        invert <- solve(mat, ...)
        x$setInverse(invert)
        invert
}

## Write a short comment describing this function
        ## Return a matrix that is the inverse of 'x'
##if you create a small matrix
smallmatrix <- makeCacheMatrix(matrix(1:4, nrow =2, ncol =2))
##then run smallmatrix$get()
smallmatrix$get()
##you will end up with a 2 by 2 matrix
##the cacheSolve(smallmatrix) command will give you the inverse
##of the smallmatrix and will give a message when already cached

