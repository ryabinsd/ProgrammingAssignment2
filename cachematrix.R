## Cache and return inverse of a matrix
## by invoking the Cache function followed by a call to Solve
## matrix is assumed to be invertible

##makeCacheMatrix is a pseudo-procedural caching mechanism
##if this is the only way to cache in R we need to move to a different language

makeCacheMatrix <- function (x=matrix())
{
  m <-NULL
  set <-function(y)
  {
    x<<-y
    m <<- NULL
  }
  get <- function() {x}
  setinverse<- function(inv) {m <<-inv}
  getinverse<- function() {m}
  
  list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
  
}

##cacheSolve takes list produced by the invocation of above mechanism
##to return cached matrix inverse or cache one if it does not exist

cacheSolve <- function(x,...)
{
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("Get cached matrix.")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data)
  x$setinverse(inv)
  inv
}

