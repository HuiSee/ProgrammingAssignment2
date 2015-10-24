## this assignment is to write a pair of functions that cache the inverse of a matrix

## Step1
## makeCacheMatrix function - creates a special "matrix", which is really a list containing a function to

## set the value of the matrix
## get the value of the matrix
## set the value of the inverse
## get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) m <<- inverse
    getinverse <- function() m
    list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)

}


## Step2
## cacheSolve function - computes the inverse of the special "matrix" returned by function above. 
## If the inverse has already been calculated (and the matrix has not changed), then cacheSolve should retrieve the inverse from the cache

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    m <- x$getinverse()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data)
    x$setinverse(m)
    m        
}


## testing
## > x = rbind(c(2, -1), c(-1, 2))
## > m = makeCacheMatrix(x)
## > m$get()
##     [,1] [,2]
##[1,]    2   -1
##[2,]   -1    2
 
##> cacheSolve(m)
##          [,1]      [,2]
##[1,] 0.6666667 0.3333333
##[2,] 0.3333333 0.6666667

##> cacheSolve(m)
##getting cached data
##          [,1]      [,2]
##[1,] 0.6666667 0.3333333
##[2,] 0.3333333 0.6666667
