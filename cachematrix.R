## As we know the speed is important in programming, matrix 
## function like multiplcation or inverse usually costly.
## So, in this programm, we are caching the data so that 
## we do not have to do the long calculations repetetively.
################################################################
##
## this function is setting the matrix and also getting the matrix
## It also calculate the inverse of matrix x.

makeCacheMatrix <- function(x = matrix()) {
  m<- NULL
  
  set <- function(y){
        x <<- y
        m <<- NULL
  }
  
  get <- function()x
  
  setInverse <- function(inverse) m <<- inverse
  
  getInverse <- function()m
  
  list(set = set, get = get, setInverse= setInverse, getInverse=getInverse)

}

###########################################################################
## If the inverse of matrix is already calculated and in the cache
## It directly use it by using getInverse. and if the inverse does 
## not executed, then it calculate and setting the inverse using 
## setInverse function
#############################################################################

cacheSolve <- function(x, ...) {
  
  m <- x$getInverse()
  
  if(!is.null(m)){
        message("getting cached data")
        return(m)
  }
  
  data <- x$get()
  m <- solve(data,...)
  x$setInverse(m)
  m
        ## Return a matrix that is the inverse of 'x'
}
