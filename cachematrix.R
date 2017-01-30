## Together these functions will create a special 'matrix' object which caches 
# its inverse.

## This function will create a special "matrix" object which is written as a list 
# containing a function. 
# This function sets the value of the matrix, gets the matrix object, sets the 
# inverse of the matrix and gets the inverse of the matrix. 
# It essentially creates a matrix object which can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
      m<-NULL
      set<-function(y) {
              x<<-y
              m<<-NULL
}
       get<-function () x
       getinverse<-function() m<<- solve
       setinverse<-function() m
       list(set=set,get=get,setinverse=setinverse,getinverse=getinverse)
}

## This function computes the inverse of the special "matrix" above, however it 
# first checks whether the inverse has been computed in the cache. 
## If the inverse has been caclulcated it is returned and the computation is 
# skipped, but if not then it will calculate the inverse of the matrix and set 
#the value of the inverse in the cache using the getinverse function.


cacheSolve <- function(x, ...) {
        
        ## Return a matrix that is the inverse of 'x'
        cachemean <- function(x, ...) {
                m <- x$getinverse()
                if(!is.null(m)) {
                        message("getting cached data")
                        return(m)
                }
                data <- x$get()
                m <- mean(data, ...)
                x$setinverse(m)
                m
        }
        }



