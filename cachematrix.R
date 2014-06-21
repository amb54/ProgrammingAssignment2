## Put comments here that give an overall description of what your functions do
        ##These functions stores a matrix, computes its inverse and store the inverse.
        ##This is to save time in case the inverse is used over and over (no need to calculate it over and over)
        ##The code follows the example given in the instuctions for Assignment2.
	##The function solve() is used instead of the function mean()


## Write a short comment describing this function
        ##This function has a matrix x as it's argument and returns a list of 4 functions
        ##used when caching the inverse of that particular matrix x

        ##The function "setMatrix" gives the object x the value of the matrix given as the argument

        ##The function "getMatrix" returns the matrix set by "setMatrix"
        ##or given as an argument for "makeCacheMatrix"

        ##The function "setInvMatrix" sets the inverse to the matrix created by "setMatrix"

        ##The function "getInvMatrix" returns the inverse matrix set by "setInvMatrix".
        ##If no inverse matrix has been set the return value is NULL.

makeCacheMatrix <- function(x = matrix()) {
  
        invMatrix <- NULL
        
        setMatrix <- function(y) {
                x <<- y
                invMatrix <<- NULL
        }
        
        getMatrix <- function(){
                x
        } 
        
        setInvMatrix <- function(solvedInv){
                invMatrix <<- solvedInv    
        } 
        
        getInvMatrix <- function(){
                invMatrix
        } 
        
        list(setMatrix = setMatrix, getMatrix = getMatrix, setInvMatrix = setInvMatrix, getInvMatrix = getInvMatrix)
     
}




## Write a short comment describing this function
        ##This function has a matrix as it's argument and return the inverse of that matrix.

        ##The function first retrive data from the list created by the function "makeCacheMatrix"
        ##by the function "getInvMatrix" for the particular matrix
        ##to check if the inverse of the matrix already have been solved.

        ##If that is the case, it returns the already solved inverse matrix
        ##together with the words "getting cached data"

        ##If that is not the case, the inverse is solved, set by the function "setInvMatrix", and then returned.

cacheSolve <- function(x, ...) {
        
        invMatrix <- x$getInvMatrix()
        
        if(!is.null(invMatrix)) {
                message("getting cached data")
                return(invMatrix)
        }
        
        mat <- x$getMatrix()
        
        invMatrix <- solve(mat, ...)
        
        x$setInvMatrix(invMatrix)
        
        invMatrix

}