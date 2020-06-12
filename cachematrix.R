## Thanks to the article of Lgreski in Demystifying makeVector() in helping to understand the steps.
## Comments below every step

makeCacheMatrix <- function(x = as.matrix()) {
  s <- NULL
  
## function: makeCacheMatrix, initialization of x and s.
## x as an empty matrix, s is set to NULL. 
  
   set <- function(y){
    x <<- y
    s <<- NULL
   }

## Assign the input argument to the x object in the parent environment, and
## Assign the value of NULL to the s object in the parent environment. 
   
 
    get <- function() x
    
## Since the symbol x is not defined 
## within get(), R retrieves it from the parent environment of makeCacheMatrix().
  
  setinverse <- function(solve) s <<- solve
  
## Since s is defined in the parent environment and we need to access it after setinverse() completes, 
## the code uses the <<- form of the assignment operator to assign the input argument to the value of 
## s in the parent environment.
  
  getinverse <- function() s
  
## get the value of the inverse, using lexical scoping to find the correct symbol s and retrieve its value.
  
  list(set = set, get = get, 
       setinverse = setinverse, 
       getinverse = getinverse)

}
## the code assigns each of these functions as an element within a list(), and returns it to the 
## parent environment.  


## Return a matrix that is the inverse of 'x'

cacheSolve <- function(x, ...) {

## cacheSolve starts with a single argument, x, and an ellipsis that allows the caller to pass additional 
## arguments into the function.
  
  s <- x$getinverse()
  if(!is.null(s)){
    message ("getting cache data")
    return(s)
  }
  
## First, it calls the getinverse() function on the input object.  
## it checks to see whether the result is NULL. Since makeCacheMatrix() sets the cached inverse (s) to NULL whenever a 
## new matrix is set into the object, if the value here is not equal to NULL, we have a valid, cached inverse 
## and can return it to the parent environment 
  
  data <- x$get()
  s <- solve(data,...)
  x$setinverse(s)
  s
  
}



