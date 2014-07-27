## Matrix inversion is usually a costly computation and their may be some benefit to caching the inverse of a matrix rather than compute it repeatedly 
## (there are also alternatives to matrix inversion that we will not discuss here). 

## Here is a pair of functions that cache the inverse of a matrix.


## This function creates a special "matrix" object that can cache its inverse.

## The first function, makeCacheMatrix creates a special "matrix", which is really a list containing a function to
## set the value of the Matrix
## get the value of the Matrix
## set the value of the inverse
## get the value of the inverse

makeCacheMatrix <- function(matrix = matrix()) 

	{
		mat_inv <- NULL

        	set <- function(y = numeric()) 
				{
                			matrix <<- y
					mat_inv <<- NULL
       			 }
       
		get <- function() matrix

      	setinverse <- function(inverse) mat_inv <<- inverse
        	
		getinverse <- function() mat_inv

			list(
				set = set, get = get,
          			setinverse = setinverse,
          			getinverse = getinverse
				)
	}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.





## The following function calculates the inverse of the special "matrix" created with the above function. 
## However, it first checks to see if the inverse has already been calculated. 
## If so, it gets the inverse from the cache and skips the computation. Otherwise, it calculates the inverse of the data and sets the value of 
## the inverse in the cache via the solve() function.

cacheSolve <- function(matrix, ...) 

	{
     	mat_inv <- matrix$getinverse()
        	
		if(!is.null(mat_inv)) 
			{
               		message("getting cached data")
                
				return(mat_inv)
        		}
        
		else 

			{

				message("computing inverse of the matrix")

				data <- matrix$get()

				matrix$setinverse(solve(data))
		
				mat_inv <- matrix$getinverse()
			}

		
		mat_inv

	   ## Return a matrix that is the inverse of 'matrix'
	}
