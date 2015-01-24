## The below functions makeCacheMatrix and cacheSolve helps to store the
## inverse of a given square determinant matrix.


## function makeCacheMatrix accepts the input as matrix and being called 
## to assign a variable of the kind of makeCacheMatrix 
## e.g a <- makeCacheMatrix(x). This prepares a namespace for each individual
## variable a with all the functions listed in the makeCacheMatrix function.

makeCacheMatrix <- function(x = matrix()) {
m <- NULL ## sets the value of m to NULL
        set <- function(y) {
                x <<- y   ## <<- sets the value of the variable to a value defined
                          ## in the calling function environment if there is no 
                          ## local value provided
                m <<- NULL
        }
        get <- function() x ## this is a function and similar to function(){x}
        setinverse <- function(solve) m <<- solve  
            ## this is a function and sets the invesrse of the matrix using 
            ## lexical scoping.
                                   
        getinverse <- function() m ## this function gets the inverse of the 
                                   ## matrix stored
        ## the below code prepars the LIST of the functions
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## The below function cacheSolve accepts variable of the type makeCacheMatrix
## e.g as mentioned the comments of the first function, a <- makeCacheMatrix(x)
## Hence if a is first time being called this will calculate and store the 
## inverse of the matrix x, if a is called upn second time it will just return
## the stored inverse matrix for x

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinverse()    ## get the inverse of the matrix stored
               if(!is.null(m)) {      ## checks for whether the inverse exist in
                                           ## the global environment 
                message("getting cached data")
                return(m)
        }                 ## end of curly braces for IF part below is the ELSE part
        data <- x$get()   ## calls the get function defined in makeCacheMatrix
        m <- solve(data, ...) ## assigns the inverse of the matrix 
        x$setinverse(m)       ## stores the inverse of the matrix by calling
                              ## function setinverse
        m
}
