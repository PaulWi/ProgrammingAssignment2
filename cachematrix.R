# A function creates a special "matrix" object that can cache its inverse for further use
makeCacheMatrix<-function(x=matrix()){
    
    i<-NULL
    
    get<-function() x  
    set<-function(xnew) {
        x<<-xnew
        i<<-NULL
    }    
    getInverse<-function() i
    setInverse<-function(inverse) i <<- inverse
    
    list(get=get, set=set, getInverse=getInverse, setInverse=setInverse)    
}

# This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
# If the inverse has already been calculated (and the matrix has not changed), then the cacheSolve 
# will retrieve the inverse from the cache.  Note that function only works for square matrices
cacheSolve<-function(x, ...){
    
    data <- x$get()
    
    i <- x$getInverse()                         # assigned using the makeCacheMatrix instance
    if(!is.null(i)) {
        message("Getting cached data ...")
        return(i)                               # exit here, if we have previously calculated this.
    }    
    i <- solve(data, ...)
    x$setInverse(i)                             # update the parent environment with this value
    i
}
