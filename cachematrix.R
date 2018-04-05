## makeCacheMatrix function creates a special matrix, and its inverse as well
##by:
# 1.set value of x matrix
# 2.get value of x matrix

# 3.set value of inverse matrix
# 4.get value of inverse matrix
makeCacheMatrix <- function(x = matrix()) 
   {
   inv_matrix <- NULL
   
   ##setting and getting x matrix 
   set <- function(dummy)
      {
      ##assigning dummy to x's environment
      x <<- dummy
      inv_matrix <<- NULL
      }
   
   get <- function() x ##returns x value
   
   ##setting and getting inverse matrix value
   setinverse <- function(inverse) inv_matrix <<- inverse
   
   getinverse <- function()  inv_matrix
   
   list(set = set, get = get,
        setinverse = setinverse,
        getinverse = getinverse)
   }



## cachSolve function calculates the inverse of x matrix passed from the above function. 
## we check value of inv_matrix to make sure it was calculated or not.

cacheSolve <- function(x, ...) 
   {
   ## this function returns inverse of x matrix 
   ##1. getting value of inverse matrix from above 
   inv_matrix <- x$getinverse()
   ##checking value of inv_matrix 
   if(!is.null(inv_matrix))
      {##inverse calculated 
      message("the inverse matrix is:")
      return(inv_matrix)
      }
         ##calculating inverse 
         xdata <- x$get() ##getting x matrix
         inv_matrix <- solve(xdata, ...) 
         x$setinverse(inv_matrix)
         print(inv_matrix)
}
