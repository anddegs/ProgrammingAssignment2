## The following functions (makeCacheMatrix,cacheColve) are fucntions
##that create a matrix and cache it thru global environment variable
##the makeCache served as a container where matrix can be set and get to 
#as an environemtvariable, as where the cacheSolve checks if the matrix was already in cache
#by referencing the makeCacheMatrix getInverse method and if it returns NULL then it will
#create and solve a matrix and set it to makeCacheMatrix object

#To verify the result
#1. load the source code -> source("cachematrix.R")
#2. create the function and set it to variable 
# w<-makeCacheMatrix()
#3.set a value 
#w$set(matrix(c(0,2,2,0), 2, 2))
#4test the cachecolve function via 
#cacheSolve(w) -> thiw will print Matrix not yet cached
#5execute again the function
#cacheSolve(w) -> thiw will print Matrix was already cached




# This function create a method where object can be set and get
# to environment variable, to make it available to other method
makeCacheMatrix <- function(mtrx = matrix()) {
  inverse <- NULL
  
  set <- function(x) {
    f <<- x;
    inverse <<- NULL;
  }
  
  get <- function() return (f);
  
  setInverse <- function(inv) {
    inverse <<- inv;
  }

  getInverse <- function() {
    #message("getting inverse")  
    return(inverse);
  }
  return(list(set = set, get = get, setInverse = setInverse, getInverse= getInverse))
}


## This function checks if the matrix was already in cache
#by referencing the makeCacheMatrix getInverse method and if it returns NULL then it will
#create and solve a matrix and set it to makeCacheMatrix object
cacheSolve <- function(mtrx, ...) {
    
    ## Return a matrix that is the inverse of 'mtrx'
    inverse <- mtrx$getInverse()
    
    if(!is.null(inverse)){
        message("Matrix was already cached")  
        return(inverse) 
    }else {
      message("Matrix not yet cached")  
    }
  
    data <- mtrx$get()
    inver <- solve(data,...)
   # message( inver )
    mtrx$setInverse(inver)
    
    inverse <- mtrx$getInverse()
    return (inverse)
    #return(inver)
    
}  
