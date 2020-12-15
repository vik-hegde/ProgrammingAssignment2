## Function to store and retrieve inverted matrix 
makeCacheMatrix <- function(x = matrix()) 
{
#Initialize a null matrix
  m <- matrix(list(NULL),nrow(x),ncol(x))
#
  set <- function(y) 
  {
    x <<- y
    m <<- matrix()
  }
  get <- function() x
#  
#Stores the inverted matrix from cachesolve function
  setinv <- function(inv) m <<- inv 
  getinv <- function() m
  list(set = set, get = get,setinv = setinv,getinv = getinv)
}
#
## Function to calculate inverted matrix
cacheSolve <- function(x, ...) 
{
  count=0
  elm_m=c()
  m <- x$getinv()
# loop to verify if each element of m is NULL or not
  elm_m<-sapply(m,is.null)
  for(i in elm_m)
  {
    if(isTRUE(i)) 
    {
      count<-count+1
    }
  }
#if m is not NULL cached m will be retrieved
  if (count!=nrow(m)^2)
  {
    message("getting cached data")
    return(m)
  }
#if m is NULL new inverse will be calculated
  data <- x$get()
  m <- solve(data)
  x$setinv(m)
  m
}
