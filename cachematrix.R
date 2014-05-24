#start 
# function that makes a list of 4 functions and an object
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL #makes an empty object
#definitions of the 4 functions:
  set <- function(y) {
    x <<- y
    m <<- NULL
  } #function overwriting the input matrix and setting object m to null, both
  # accessibe from outside the local function environment 
#(so that they can be altered by another function)
  get <- function() x #function returning input matrix
  setinv <- function(solve) m <<- solve # function assigning an argument "solve" 
  #(can be substituted for any other name, e.g. "z") to m accessible to be altered
#from outside of the local function environment 
  getinv <- function() m # function returning  m
  list(set = set, get = get, #makes a list object of the 4 functions defined above
       setinv = setinv,
       getinv = getinv)
}

# caches the inverse matrix 
cacheSolve <- function(x, ...) {
  m <- x$getinv() #assigns (stored inverted matrix) to m 
  if(!is.null(m)) { #and checks if m is null. If it isn't,
    message("getting cached data") #says it looks for data and
    return(m) #returns what was stored
  }
  data <- x$get() #creates the argument for the solve() function - recalls input matrix
  m <- solve(data, ...) #calculates inverse matrix and assigns it to m object (in the environment of the function) 
  x$setinv(m) #assigns newly calculated inverse matrix to m accessible outside and
  m #returns it
} 


# it worked for me only when what went into the
# "makeCacheMatrix() function was created beforehand as a 
# square matrix, for example:

## d<-matrix(1:4,2,2)
## a<-makeCacheMatrix(x=d)
## f<-matrix(5:8,2,2)
## b<-makeCacheMatrix(x=f)
## cacheSolve(a)
## cacheSolve(b)
## cacheSolve(a)
## cacheSolve(b)

#the end
