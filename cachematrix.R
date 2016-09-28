## Put comments here that give an overall description of what your
## functions do

## This function that I've written is modelled after the makeVector and Cachemean functions
## that have been given as examples. The main value addition in my opinion is the code
## documentation, which I hope describes how the code runs, how the two different functions
## interact with each other, and how lexical scoping comes into play here


# Running this function in this way - a matrix is defined in a variable, and the matrix
# would be invertible
# The matrix would be passed to the function makeCacheMatrix, which would then create the
# objects in the list

makeCacheMatrix <- function(x) {
      inv <- NULL
      # The variable inv is initialized as NULL initially, when the function is first defined
      
      set <- function(y){
            x <<- y        # <<- symbol signifies that this value of x is saved in the
                           # parent function      
            inv <<- NULL
      }
      # The set function is the first of four functions defined within this function
      # Although 'set' itself is never called, calling set directly you can initialize the
      # matrix, without the need to call the entire makeCacheMatrix function
      
      get <- function() x
      
      # This function is called when the cacheSolve function wants the original matrix from
      # the makeCacheMatrix function - it returns the matrix intact to the variable 'temp'
      # in cacheSolve, and the variable 'temp' is passed to the function 'Solve', which in
      # the r library finds the determinant of the matrix in 'temp'
      
      setinverse <- function(inverse) inv <<- inverse
      
      # Once the inverse matrix has been determined in the cacheSolve function, the inverse
      # matrix is passed to setinverse, which sets the variable inv to the inverse matrix 
      # passed by the function. The 'inv' variable now holds a matrix instead of NULL
      
      
      getinverse <- function() inv
      
      # This function is the first function called by cacheSolve, which if not null returns
      # the cached inverse, and otherwise carries out the computation of the inverse
      
      
      list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
      
      # The list object is created. Each of the names of variables are the same as the
      # function whose value they hold. I am not entirely clear why the elements of list
      # are named, only that if they weren't and instead only the functions were mentioned
      # as elements of the list it would give up an error
      
      
}








## This is the main function that is called, whose argument is the special list object
## created by the makeCacheMatrix function. Each function called in this matrix is explained
## in the description of the function

cacheSolve <- function(x, ...) {
      inv <- x$getinverse()   ## calls getinverse, returns inverse if the inverse has been
                              ## cached
      if(!is.null(inv)){
            print("Getting cached inverse")
            return(inv)
      }
      # The if loop returns the inverse if the inverse has been cached. If not the remaining
      # code in the function is executed
      
      temp <- x$get()
      # the matrix is returned to the temp variable, as the get() function returns the matrix
      
      inv <- solve(temp, logarithm = FALSE,...)
      # The inv of the matrix is computed here
      
      x$setinverse(inv)
      # The inverse calculated and stored in 'inv' within the cacheSolve function is now
      # argument to this function which is called, and which is then stored in the 'inv' 
      # variable in the makeCacheMatrix function. The inverse matrix is therefore cached
      # for future use
      
      inv
      # The inverse matrix is returned finally.
}
