# The two functions below, together,create an inverse of a matrix, only if the 
#inverse has not been created earlier

#Step 1: A square matrix has to be first passed into makeCacheMatrix.
# which just stores the matrix in a cache. The resulting list from this function
#should be stored in a variable

#Step 2: CacheSolve has to be passed the output variable from Step1. If it is 
#the first time that the solve is running on the matrix, a fresh inverse will be
#calculated. Else a cached inverse will be retured.

# store a matrix in a cache. Also generated functions to receive/return inverse
makeCacheMatrix <- function(x = matrix()) {
    i<-NULL
    set<-function(y){
        x<<-y
        i<<-NULL
    }
    get<-function() x
    setinverse<-function(inverse) i<<-inverse
    getinverse<-function() i
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)

}



##solves for inverse, by taking in a list from the makeCache function
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    i<-x$getinverse
    if(is.null(i)){
        message("getting cached data")
        return(i)
    }
    data<-x$get()
    i<-solve(data)
    x$setinverse(i)
    i
}
