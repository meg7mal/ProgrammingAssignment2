## The following functions help us create a copy of a supplied invertible square matrix for which the inverse once 
## calculated, will be cached and reused if we attempt to calculte the inverse of the same copy.

## This function creates a copy "X" of an invertible square matrix "supplied_square_matrix". It also returns
## a list of functions that can be called on this copy: set_matrix, get_matrix, set_inverse, get_inverse

makeCacheMatrix <- function(x = matrix()) {
      
      inverse<-NULL
      prev<-NULL ## keeps track of whether there has been a change in the value of the matrix
      
      set_matrix<-function(supplied_square_matrix){
                        
              if(!is.null(prev)){
                      if(!identical(prev,supplied_square_matrix)){
                              x<<-supplied_square_matrix
                              inverse<<-NULL
                              prev<<-x
                      }
              }
              else{
                      x<<-supplied_square_matrix
                      inverse<<-NULL
                      prev<<-x
              }
        
      }
      
      
      get_matrix<-function() x
       
      set_inverse<-function(inv) inverse <<-inv
      
      get_inverse<-function() inverse
 
      
      list(set_matrix = set_matrix, get_matrix=get_matrix,
           set_inverse = set_inverse,
           get_inverse = get_inverse)
      
}


## When matrix created by makeCacheMatrix is passed to this function, there is a check
## to see if the inverse has already been set (by set_inverse()). If it has been, cacheSolve
##will re-use the value from cache else the inverse will be calculated.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        
        inverse <- x$get_inverse()
        if(!is.null(inverse)){
                message("getting cached data")
                return(inverse)
        }

        data <- x$get_matrix()
        inverse <- solve(data)
        x$set_inverse(inverse)
        inverse
               
        
}