## We are creating two functions. They work together. The first
#creates the R object to store a matrix and its inverse. The second -
# cachesolve function uses the inverse created in the first function
#and retrieves it from the cache if it exists.

## Write a short comment describing this function
#
##define x as an empty numeric vector
makeCacheMatrix <-function(x =numeric()) {
  #set m to null as a placeholder for future value
  m<- NULL
  #The next step is to define a function to set the vector x to a new vector
  #y and reset the mean m to null.
  #When set is executed it; assigns the input argument
  #to the x object in the parent environment, and assigns the value of null
  # to the m object in the parent environment. Therefore, if there's a
  # valid inverse already cached in m this will be cleared and recalculated
  #for the new input matrix.
  set <-function(y){
    x<<-y
    m<<-NULL
  }
  #since x is not defined within get(), R retrieves it from the parent
  #environment of makeCacheMatrix.
  get<-function() x
  #since m is defined in the parent environment we need to access it after
  #setinverse completes. We use the <<- to assign the inpout argument to the
  #value of m in the parent environment. We use the function solve() to
  #generate the inverse.
  setinverse<-function(solve) m<<-solve
  #as above getinverse finds the correct m to retreive its value
  getinverse<-function() m
  #assign each function we created as an element within a list and
  #return it to the parent environment.
  list(set=set,get=get,
       setinverse=setinverse,
       getinverse=getinverse)
}
#this last part gives us a fully formed object of type makeCacheMatrix()
#to be used by cachesolve.
#Because we named the elements in the list we can use $ to extract them.

## Create a function to find the inverse, call it cachesolve.
cachesolve <- function(x, ...) {
  #retrieve the inverse matrix, check if it's null. If it isn't then
  #return the valid inverse m.
  m<- x$getinverse()
  if(!is.null(m)){
    message("getting cached data")
    return(m)
  }
  #execute the solve() function. This is necessary to generate the inverse.
  data<-x$get()
  m<-solve(data, ...)
  x$setinverse(m)
  m
}

#Now we have run both pieces of code, check the results are as expected
#with some examples.
mmat<-makeCacheMatrix()
x<-matrix(rnorm(100),10,10)
x<-matrix(1:4,2,2)
x<-matrix(5:8,2,2)

mmat$set(x)
mmat$get()
mmat$getinverse()
cachesolve(mmat)

