## The two functions below can compute and chaches the Inverse of a Matrix.


## This function caches the inverse of special "matrix"


makeCacheMatrix <- function(x = matrix()) {
    k<-NULL
    set<-function(y){
      x<<-y
      k<<-NULL
    }
    get<-function() x
    setmatrix<-function(solve) k<<- solve
    getmatrix<-function() k
    list(set=set, get=get,
         setmatrix=setmatrix,
         getmatrix=getmatrix)
  }



## This function calculate the inverse of Matrix provided by makeCacheMatrix()

cacheSolve <- function(x, ...) {
  
  ## Return a matrix that is the inverse of 'x'
    k<-x$getmatrix()
  if(!is.null(k)){
    message("Getting Cached Data")
    return(k)
  }
  matrix<-x$get()
  k<-solve(matrix, ...)
  x$setmatrix(k)
  k
}
