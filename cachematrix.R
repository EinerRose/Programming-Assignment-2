
makeCacheMatrix <- function(x = matrix()) {    
makeVector <- function(x = numeric()) ##define the argument with model matrix
inverse <- NULL  ##the value of matrix inverse
set <- function(y) {  ##assign a new variable with function(y)
       x <<- y      ##value of matrix
       m <<- NULL   ##if it's a new matrix, reset value to NULL
}
get <- function() x ##assign a new variable with function(x)
setinverse <- function(i) inverse <- i  ##assign a new variable with function(inverse), reset m avalue to inverse
getinverse <- function() inverse   
list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)  ##we need this to run the function within the list
}


cacheSolve <- function(x, ...) {  ##cacheSolve is assigned by a new function, if the function works, the result will be calculated 
        ## Return a matrix that is the inverse of 'x'
        inverse <- x$getinverse()
        if(!is.null(inverse)) {
                message("getting cached inverse")
                return(inverse)
        }
        data <- x$get()
        inverse <- solve(data, ...)
        x$setinverse(inverse)
        inverse
}
