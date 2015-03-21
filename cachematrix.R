## makeCacheMatrix: Makes a Matrix a CacheMatrix with the data stored in cache
## cacheSolve: Solves the inverse of a Cache Matrix looking if the inverse is already stored in cache

## Converts a Matrix to a CacheMatrix (stores the matrix and the inverse in cache)
makeCacheMatrix <- function(x = matrix()) {
        
        ## By default, the inverse "matrix" is set to null until is calculated
        matrix <- NULL
        
        ## SET() will store the matrix that we want to inverse in cache
        set <- function(y) {
                x <<- y
                
                ## If we are changin the matrix, we have to set the inverse to null
                matrix <<- NULL
        }
        
        ## GET() will retrieve the matrix that we want to inverse
        get <- function() x
        
        ## SETINVERSE() stores the inverse "matrix" in cache
        setinverse <- function(inverse) matrix <<- inverse
        
        ## GETINVERSE() retrieves the inverse "matrix" from cache
        getinverse <- function() matrix
        
        ## This list returns the functions that can be used
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## Solves the inverse of a CacheMatrix
cacheSolve <- function(x, ...) {
        ## We look if the inverse was already stored in cache
        matrix <- x$getinverse()
        
        ## If the inverse was stored before, then "matrix != null"
        if(!is.null(matrix)) {
                ## We return the inverse that is in the "matrix" cache
                message("getting cached data")
                return(matrix)
        }
        
        ## If the inverse was not stored before, then "matrix == null"
        
        ## PROCESS OF CALCULATING THE INVERSE AND STORING IT IN CACHE
        ## Step 1. We retrieve from the cache the matrix that we want to invert
        data <- x$get()        
        ## Step 2. We solve the matrix
        matrix <- solve(data, ...)
        ## Step 3. We store the inverse in cache
        x$setinverse(matrix)
        ## Step 4. We return the inverse that is now stored in the "matrix" cache
        matrix
}

