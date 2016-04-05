# makeCacheMatrix creates a list containing a function to
# set the value of the matrix
# get the value of the matrix
# set the value of inverse of the matrix
# get the value of inverse of the matrix

# makeCacheMatrix function

makeCacheMatrix <- function(x = matrix()) {
i=NULL
set<-function(y){
x<<-y
i<<-NULL
}
get<-function()x
setinverse<-function(inv)i<<-inv
getinverse<-function()i
list(set=set,get=get,
setinverse=setinverse,
getinverse=getinverse)
}

# The following function returns the inverse of the matrix. It first checks if
# the inverse has already been computed. If so, it gets the result and skips the
# computation. If not, it computes the inverse, sets the value in the cache via
# setinverse function

#This function assumes that the matrix is always invertible.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        i<-x$getinverse()
        if(!is.null(i)){
        message("getting catched data")
        return(i)
        }
        m<-x$get()
        i<-solve(m,...)
        x$setinverse(i)
        i
        }
        
