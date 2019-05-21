## these two posts were helpful:  https://github.com/ecxr/RProg/blob/master/proj2/cachemean.R
## and https://github.com/lgreski/datasciencectacontent/blob/master/markdown/rprog-breakingDownMakeVector.md
##stil, i think the 'encloding' and 'parent' talk could be understood in more than one way; in other words, it was
##ambiguous.  I would say 'all behaviors and data that are instantiated earlier in execution are available to the 
##function that calls them, i.e. the 'parent' function.  do you agree?  suggestions?  corrections?  also, makeCacheMatrix
##was designed to be executed within another function, which was hard to wrap my head around; the thought 
##'coded inside out' flashed vaguely. '<<' means the value being set comes from the calling function, i.e. outside.
##btw, the set inverse method is only executed inside cacheSolve.  the set functions are strange, which is why i gave
## them the 'whatever' names. 

##makeCacheMatrix has the data and get/set behaviors used by cacheSolve to get and set values within makeCacheMatrix

makeCacheMatrix <- function(x = matrix()) {
  invMat <- NULL
  set <- function(NameWhatever1) {
    x <<- NameWhatever1
    invMat <<- NULL
  }
  get <- function() x
  setInverse <- function(NameWhatever2) invMat <<- NameWhatever2
  getInverse <- function() invMat
  list(set = set,
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}



cacheSolve <- function(x, ...) {
  invMat <- x$getInverse()
  if (!is.null(invMat)) {
    message("getting cached data")
    return(invMat)
  }
  mat_data <- x$get()
  invMat <- solve(mat_data, ...)
  x$setInverse(invMat)
  invMat
}

my_matrix <- makeCacheMatrix(matrix(c(159,222,987,456), 2, 2))
my_matrix$get()
my_matrix$getInverse()

cacheSolve(my_matrix)


my_matrix$getInverse()

my_matrix$set(matrix(c(2, 2, 1, 4), 2, 2))
my_matrix$get()

my_matrix$getInverse()

cacheSolve(my_matrix)
