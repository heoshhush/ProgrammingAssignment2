
# makeCacheMatrix: This function creates a special 
#"matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y){
                x <<- y
                i <<- NULL
        }
        get <- function() x
        setinver <- function(inverm) i <<- inverm
        getinver <- function() i
        list = list(set = set, get = get, setinver = setinver, 
                    getinver = getinver)
}



## WcacheSolve: This function computes the inverse of 
#the special "matrix" returned by makeCacheMatrix above.

cacheSolve <- function(x, ...) {
        i <- x$getinver()
        if(!is.null(i)){
                message("getting cache data")
        }
        
        data <- x$get()
        i <- solve(data,...)
        x$setinver(i)
        i
   
}
