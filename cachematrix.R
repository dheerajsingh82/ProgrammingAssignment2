## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## ## This Function makeCacheMatrix gets a matrix as an input, set the value of the matrix,creates a matrix object  as ainverse matrix object

makeCacheMatrix <- function(x = matrix()) {
        ## define the argument with default mode of "matrix"
        inv <- NULL                            
        
        set <- function(y) { 
                x <<- y  
                
                inv <<- NULL   
        }
        get <- function() x                     
        
        ## define the get fucntion - returns value of the matrix argument
        
        setinverse <-  function(inverse)
                
                inv <<- inverse  ## assigns value of inv in parent environment
        
        getinverse <- function()
                inv                     
        list(   set = set,
                get = get,
                setinverse = setinverse,
                getinverse = getinverse )  
}




## Write a short comment describing this function
### This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not changesd), then cacheSolve should retrieve the inverse from the cache

cacheSolve <- function(x, ...) {
        
        inv <- x$getInverse()
        
        if(!is.null(inv))
        {
                message("getting cached data")
                return(inv)
        }
        
        data <- x$get()
        inv <- solve(data)
        x$setInverse(inv)
        inv      
}


