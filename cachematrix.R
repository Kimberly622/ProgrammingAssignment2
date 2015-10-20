## The following two functions cache the inverse of a matrix 

## makeCacheMatrix creates a list containing a function to 
# 1. set the value of the matrix 
# 2. get the value of the matrix 
# 3. set the value of the inverse 
# 4. get the value of the inverse

makeCacheMatrix <- function(x=matrix()){
  m <- NULL 
  set <- function(y){
    x <<-y 
    m <<- NULL
  }
  get <-function()x
  setinverse <-function(inverse) m <<- inverse
  getinverse <-function() m
  list(set=set, get=get,
       setinverse=setinverse,
       getinverse=getinverse)
}       

## cacheSolve computers the inverse of the matrix returned by makeCacheMatrix

cacheSolve <- function(x,...){
  m <- x$getinverse()
  if(!is.null(m)){
      message("getting cached data")
      return(m)
  }
  data <-x$get()
  m<-solve(data,...)
  x$setinverse(m)
  m
}
 
