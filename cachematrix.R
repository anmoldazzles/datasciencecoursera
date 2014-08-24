## This function calculates the inverse of the matrix.We have made 4 functions get(),set(),getssolve(),setsolve() which will be returned by the make Cachematrix function
## The cache solve function calculates the inverse of the matrix.If a new matrix is entered,it sets the value of the new matrix & then calculates inverse.If no new matrix is entered,it shows the already calculated inverse.



makeCachematrix<- function(x = matrix())
  {
  m <- NULL
  
  set <- function(y) 
    {
    x <<- y
    m <<- NULL
    }
              
  get <- function() 
    {x}
  
  setsolve <- function(solve)
    {
    m <<- solve
    }
  
  getsolve <- function()
    {m}
  
  list(set = set, get = get,setsolve = setsolve,getsolve = getsolve)
   }

##Inverse of a matrix

cachesolve <- function(x, ...)
  {
  
  ##Return a matrix that is the inverse of x
  
  m <- x$getsolve()
  if(!is.null(m)) 
    {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setsolve(m)
  m
}
