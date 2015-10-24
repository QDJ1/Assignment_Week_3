
###  R PROGRAMMING - ASSIGNMENT WEEK 3 
###
### Author  : QDJ
### Date    : 24/10/2015
### Purpose : Assignment: Caching the Inverse of a Matrix
### Filename: cachematrix.R
### Version : v1.0

## Description: Creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
  matInv<-NULL
  set<-function(m1){ 
    x<<-m1 
    matInv<-NULL
  }
  get<-function(){ x }
  setMatInv<-function(MInv){ matInv<<-MInv }
  getMatInv<-function(){ matInv }
  list(set=set,get=get,setMatInv=setMatInv,getMatInv=getMatInv)
}


## Description: Computes the inverse of the special "matrix" returned 
##              by makeCacheMatrix above. If the inverse has already 
##              been calculated (and the matrix has not changed), then 
##              the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  matInv<<-x$getMatInv()
  if(!is.null(matInv)){
    return(matInv)
  }
  data<-x$get()
  matInv<-solve(data,...)
  x$setMatInv(matInv)
  matInv
}
