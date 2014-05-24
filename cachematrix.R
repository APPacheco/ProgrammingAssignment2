## Matrix inversion is usually a costly computation and their may be some benefit to caching
## its inverse rather than compute it repeatedly.


## This function creates a special "matrix" object that can cache its inverse.
## It creates a vector for inputting to the cacheSolve() function which sets and gets
## the value of the matrix and its inverse (also a matrix)

makeCacheMatrix <- function(x = matrix()) {
      m <- NULL  # cache
      
      # Function that stores the matrix
      set <- function(y) {
            x <<- y
            m <<- NULL
      }

      # Function that returns the matrix
      get <- function() x
      
      # Function that stores the matrix inverse
      savecache <- function(value) m <<- value
      
      # Function that returns the matrix inverse
      getcache <- function() m
      
      # return a list with the result of the four functions
      list(set=set, get=get, savecache=savecache, getcache=getcache)
}




## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
## If the inverse has already been calculated (and the matrix has not changed),
## then the cachesolve should retrieve the inverse from the cache.


cacheSolve <- function(x, ...) {
      # Check if we have a a square matrix as input
      if (nrow(x$get())!=ncol(x$get())) {
            stop("Please give a square matrix")
      }
      
      m1 <- x$getcache()
      
      # If we have this matrix available in cache (produced by a previous call),
      # return the stored value (instead of doing the calculation)
      if(!is.null(m1)) {
            message("getting the cached inverted matrix")
            return(m1)
      }
      
      # If it is a new matrix (not stored in cache) we create the inverted matrix,
      # we store its value in makeCacheMatrix(), and we return its value
      data <- x$get()
      m2 <- solve(data, ...)  # Return a matrix that is the inverse of 'data'
      x$savecache(m2)
      m2
}


rm(list=ls())
# After sourcing the functions:  
a <- makeCacheMatrix()                # initialize

mv <- c(1,0,5,2,1,6,3,4,0)
mv <- matrix(mv,3,2)
a$set(mv)
cacheSolve(a)

mv <- c(1,0,5,2,1,6,3,4,0)
mv <- matrix(mv,3,3)
det(mv)
a$set(mv)
cacheSolve(a)
Inv <- cacheSolve(a) %*% a$get()  
Inv
round(Inv, digits = 0)



mv <- seq(2,17,2)
det(matrix(mv,3,3))
a$set(matrix(mv,3,3))
cacheSolve(a)
cacheSolve(a)
Inv <- cacheSolve(a) %*% a$get()  
Inv
round(Inv, digits = 0)



mv <- c(1,0,5,2,1,6,3,4,0,7,4,8,1,6,2,8,3,5,3,7,4,5)
det(matrix(mv,10,10))
a$set(matrix(mv,10,10))            # set the matrix
a$get()                            # get the matrix 
cacheSolve(a)                      # calculate the inverse matrix 
Inv <- cacheSolve(a) %*% a$get()  
Inv
round(Inv, digits = 0)
