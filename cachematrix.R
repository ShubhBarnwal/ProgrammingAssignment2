## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

## Making a cache matrix that stores and gets the matrix 
## And also stores and gets the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
    inv<- NULL
    set<-function(y){
        x<<-y
        inv<<-NULL
    }
    get<- function() x
    setinverse<- function(inverse){inv<<-inverse}
    getinverse<- function() inv
    list(set = set, get = get,
         setinverse = setinverse, getinverse = getinverse)

}


## Write a short comment describing this function

## Creates the cache of the matrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    invr<- x$getinverse()
    if(!is.null(invr)){
        message("Getting cached data:")
        return(invr)
    }
    mat<- x$get()
    invr<- solve(mat, ...)
    x$setinverse(invr)
    invr
}
