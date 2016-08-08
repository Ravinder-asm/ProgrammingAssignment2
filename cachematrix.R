## Caching the Inverse of a Matrix:
## Matrix inversion is usually a costly computation and there may be some 
## benefit to caching the inverse of a matrix rather than compute it repeatedly.
## Below are a pair of functions that are used to create a special object that 
## stores a matrix and caches its inverse.

## This function creates a object by the name "Matrix" that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  Mat<-NULL
  set<-function(y){
    x<<-y
    Mat<<-NULL
  }
  get<-function() x
  setmatrix<-function(solve) Mat<<- solve
  getmatrix<-function() Mat
  list(set=set, get=get, setmatrix=setmatrix, getmatrix=getmatrix)
}


## This function computes the inverse of the "matrix" created by 
## makeCacheMatrix above. If the inverse has already been calculated (and the 
## matrix has not changed), then it should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  Mat<-x$getmatrix()
  if(!is.null(Mat)){
    message("Getting Cached Data")
    return(Mat)
  }
  matrix <- x$get() 
  Mat<-solve(matrix, ...)
  x$setmatrix(Mat)
  Mat
}
