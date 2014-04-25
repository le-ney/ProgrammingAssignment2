#the makeCacheMatrix is a function which takes an argument x of type of matrix
#It returns an creates a special matrix which is a list 
#with 4 items where each item is a function wrapped 

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        if(nrow(x) != ncol(x)){ # if input matrix is not square return null
                message("Erorr: the input must be a square matrix")
                return(i)
        }
        set <- function(y){ 
                x <<- y    # check parent environments for an existing definition
                i <<- NULL 
        }
        get <- function() x 
        setinverse <- function(solve) i <<- solve
        getinverse <- function() i
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}

#The cacheSolve function uses makeCacheMatrix function in its implementation. 
#It takes an argument x of type of special matrix (output of the makeCacheMatrix function). 
#The out of this function is the inverse matrix which either is retrieved from cache or computed.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        i <- x$getinverse()
        if (! is.null(i)){
                message("retrieving cached inverse matrix")
                return(i)
        }
        mtrx <- x$get() 
        i <- solve(mtrx, ...)
        x$setinverse(i)
        return(i)    
}