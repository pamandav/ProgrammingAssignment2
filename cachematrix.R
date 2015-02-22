## makeCacheMatrix - creates a special matrix object whoch can have a refernce to
## inverse of the matrix.
## cacheSolve - Computes the inverse of a  matrix using caching mechanism.
## If the inverse if not cached, then the inverse is cached, so that next time
## it can fetch the inverse from the cache

## This function creates a special mattrix with functions
## set() , get(), setinverse() and getinverse()

makeCacheMatrix <- function(x = matrix()) {
       
        ##creates an inverse in another environment 
        globalinverse <- NULL
        
        ##sets the matrix
        set <- function(y) {
                x <<- y
                globalinverse <<- NULL
        }
        
        ##returns the usual matrix.
        get <- function() {
               x    
        }
        
        ##sets the inverse
        setinverse <- function(inverse) globalinverse <<- inverse
        
        ##fetches the inverse of the matrix
        getinverse <- function() globalinverse
        
        ##Set the list of attributes
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## This function generates the inverse of the matrix which is passed
## and stores the value in cache. Next the function is called, the value is fetched 
## from the cache.

cacheSolve <- function(x, ...) {
        ##Fetch the inverse of the matrix
        inverse<-x$getinverse()
        
        ##If the inverse is computed fetch it from the cache.
        if(!is.null(inverse)){
                message("Getting cached inverse")
                return(inverse)
        }
     
        input <-x
        ##Fetch the numeric matrix
        x<- x$get()
        
        ##Compute the inverse
        inverse <-solve(x)
        
        ##Set the inverse of the matrix
        input$setinverse(inverse)
        
        ## Return a matrix that is the inverse of 'x'
        inverse
        
}