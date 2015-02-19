## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## Create my own kind of "matrix" (which is actually a list).
## This list contains the input matrix x itself and functions to set it (set), retrieve it (get) and to calculate the inverse 'inv' (setinv,getinv)

makeCacheMatrix <- function(x = matrix()) {
   inv <- NULL # this is going to store the inverse of x
   set <- function(y) {
       x <<- y
       inv <<- NULL
   }
   get <- function() x # function to get the original matrix from this construct
   setinv <- function(solve) inv <<- solve # function to calculate the inverse matrix and make it also outside the funtion's environment available
   getinv <- function() inv # function to call the inverse matrix
   list(set=set, get=get, setinv = setinv, getinv = getinv) #list of functions that can be called
   

}


## Write a short comment describing this function
## The following function calculate the inverse of a given "matrix" x, where x is the
## special matrix created via makeCacheMatrix().
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    inv <- x$getinv()
    if(!is.null(inv)) { # if there is already an inverse
        message("inverse already calculated, getting cached data")
        return(inv)
    }
    data <- x$get() # extracting the original matrix from the construct
    inv <- solve(data) # applying solve() to calculate its inverse
    x$setinv(inv) # writing the inverse matrix into the construct
    inv # return the inverse matrix
}
