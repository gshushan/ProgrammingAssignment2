## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
    # initate the inverse matrix to NULL
    invmat <- NULL 
    
    # set the value of matrix to x and store it
    set <- function(y) { 
        x <<- y
        invmat <<- NULL
    }
    
    # return/get the original matrix
    get <- function(){
        x
    } 
    
    #set the inverse matrix to invmat
    setinverse <- function(inversemat) {
        invmat <<- inversemat 
    }
    
    # get the inverted matrix
    getinverse <- function(){
        invmat  
    } 
    #create a list of functions
    list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
    #get the inverse matrix
    invmat <- x$getinverse()
    #check if x is not null, meaning it was calculated
    if(!is.null(invmat)) {
        message("getting cached data.")
        return(invmat)
    }
    #if it was not calculates
    rawmatrix <- x$get() #get the matrix
    invmat <- solve(rawmatrix) #  calculate the inverse
    x$setinverse(invmat) #set x to the inverse matrix
    invmat #the returned value
}
