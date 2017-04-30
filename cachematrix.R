##first function

makeCacheMatix <- function(x = matrix()) {
        ##create m 
        m <- NULL
        ## create matix
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        ## get matrix
        get <- function() x
        ## set inverse function
        setinverse <- function(inverse) m <<- inverse
        ## get and return inverse matrix
        getinverse <- function() m
        ## return all variables
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## Second function

cachesolve <- function(x, ...) {
        ## get inverse matrix
        m <- x$getinverse()
        ## check is already done
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        ## get matrix
        data <- x$get()
        ##perform inverse
        m <- solve(data) %*% data
        ##set x to inverse
        x$setinverse(m)
        ##return m
        m
}
