## Below are two functions that are used to cache the inverse of a matrix


## `makeCacheMatrix`: This function creates a special "matrix" object
## that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        invr <- NULL    ##  invr will store our inverse matrix and it's reset
                        ##  to NULL every time makeCacheMatrix is called

        get <- function() { x }   ## this function returns the value of the 
                                  ## original matrix
        
        setinvr <- function(solve)  ##  this is called by cacheSolve() during 
        { invr <<- solve }          ##  the first cacheSolve() access and it 
                                    ##  will store the value using 
                                    ##  superassignment
        
        getinvr <- function() { invr } ##  this will return the cached value to 
                                       ##  cacheSolve() on subsequent accesses
        
        
        list(get = get,          ##  This list is returned with the newly created       
             setinvr = setinvr,  ##  object. It lists all the functions 
             getinvr = getinvr)  ##  ("methods") that are part of the object.
                                 ##  If a function is not on the list then it 
                                 ##  cannot be accessed externally.
}


## `cacheSolve`: This function computes the inverse of the special
## "matrix" returned by `makeCacheMatrix` above. If the inverse has
## already been calculated (and the matrix has not changed), then
## `cacheSolve` will retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        
        # the input is an object created by makeCacheMatrix
        invr <- x$getinvr()     ##  accesses the object 'invr' and gets the value
        if(!is.null(invr)) {    ##  of the inverse if it was already cached
                                ##  (not NULL) ...
            message("getting cached data")  ##  send this message to the console
            return(invr)                    ##  and return the inverse.
        }
        
        data <- x$get()           ##  we reach this code only if x$getinvr()
        invr <- solve(data, ...)  ##  returned NULL if invr was NULL then we
        x$setinvr(invr)           ##  have to invert(solve) the matrix and store
                                  ##  the result in x (see setinvr() in
                                  ##  makeCacheMatrix) 
        invr                      ##  return the matrix to the code that
                                  ##  called this function.
}
