
makeCacheMatrix <- function(x = matrix()){
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



###

cacheSolve <- function(x, ...){
        i <- x$getinver()
        if(!is.null(i)){
                message("getting cache data")
        }
        
        data <- x$get()
        i <- solve(data,...)
        x$setinver(i)
        i
}
