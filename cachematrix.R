## Matrix inversion is usually a costly computation and their may be some benefit to caching
## its inverse rather than compute it repeatedly.



## This function creates a special object that can cache a matrix and its inverse.
## It creates a vector for inputting in the cacheSolve() function which sets and gets
## the value of a matrix and its inverse (also a matrix)

makeCacheMatrix <- function(x = matrix()) {
      m <- NULL  # cache
      
      # This function stores the imputed matrix 
      set <- function(y) {
            x <<- y            # update 'x' in the parent environment
            m <<- NULL         # reset 'm' in the parent environment
      }

      # This function returns the imputed matrix
      get <- function() x
      
      # This function stores the matrix inverse
      savecache <- function(value) m <<- value
      
      # This function returns the matrix inverse
      getcache <- function() m
      
      # return a list with the result of the four functions
      list(set=set, get=get, savecache=savecache, getcache=getcache)
}





## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
## If the inverse has already been calculated (the matrix has not changed),
## then the cachesolve() should retrieve the inverse from the cache produced by makeCacheMatrix() .

cacheSolve <- function(x, ...) {
      # Check if we have a a square matrix as input
      if (nrow(x$get())!=ncol(x$get())) {
            stop("Please give a square matrix")
      }
      
      m <- x$getcache()
      
      # If we have this matrix available in cache (produced by a previous call),
      # return the stored value (instead of doing the calculation)
      if(!is.null(m)) {
            message("getting the cached inverted matrix")
            return(m)
      }
      
      # If it is a new matrix (not stored in cache) we create the inverted matrix,
      # we store its value in makeCacheMatrix(), and we return its value
      data <- x$get()
      m <- solve(data, ...)  # Return a matrix that is the inverse of the matrix 'data'
      x$savecache(m)         # store its value for future use
      m
}


