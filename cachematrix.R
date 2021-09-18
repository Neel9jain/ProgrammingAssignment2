## These functions are written to solve the inverse of a square matrix
## Since the inverse calculation only possible on non singular matrix user
## may please input only non singular matric i.e matrix with determinant != 0


## Version 1.0


## This function sets and stores the matrix and its inverse if available 


makeCacheMatrix <- function(x = matrix()) {
        mtrx <- NULL
        
        set <- function(y)   ## set the value of the matrix 
        {
                x <<- y
                mtrx <<- NULL
        }
        
        get <- function()  x ## retrieve the value of the matrix 
        setinverse <- function(inverse) mtrx<<- inverse
        getinverse <- function() mtrx
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## Cache solve function is checking if the inverse matrix of the requested 
## matrix is already available in the cache since the computation of the 
## matrix is costly process 

## Here one more error handling for handling the inverse matrix calculation  
## error has been introduced since the inverse calculation is only possible for 
## non singular matrices 


cacheSolve <- function(x, ...) {
        mtrx <- x$getinverse() ## store the value of the matrix in the mtrx var 
        if (det(x$get())== 0) ##getting the determinant of the matrix for checking singu
        {
          print ("Inverse computation is not possible for singular matrix")
        }
        else 
        {
             if(!is.null(mtrx)) ## if the invesrse is already available skip computation
                 {           
                 message("getting cached data")
                 return(mtrx)
                 }
                 data <- x$get()             ## get the matrix for calculation 
        
                 mtrx <- solve(data, ...)    ## Solve the matrix if not available 
        
                 x$setinverse(mtrx)          ## store the result in cache 
        
                 mtrx                         
                 }
}


