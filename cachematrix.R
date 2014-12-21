## use the makeCacheMatrix function to make a makeCacheMatrix object and then
## call the cacheSolve function with the object created by makeCacheMatrix
## to get the inverse of the invertible matrix that was supplied to 
## makeCacheMatrix

## here is an example of an invertible matrix that could be used to test it
## my_matrix <- matrix(c(2,3,1,5,1,0,3,1,0,2,-3,2,0,2,3,1), 4,4,byrow=TRUE)

## for makeCacheMatrix, supply an invertible matrix and assign the function to a
## variable that will point to an object that is a list of the 4 functions defined

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setmatrix_inv <- function(matrix_inv) m <<- matrix_inv
        getmatrix_inv <- function() m
        list(set = set, get = get,
             setmatrix_inv = setmatrix_inv,
             getmatrix_inv = getmatrix_inv)
}

## for cacheSolve, supply the object created by makeCacheMatrix, and cacheSolve
## will return the inverse of the matrix supplied to makeCacheMatrix, but if it
## already exists, it will return that value which has been stored in 
## makeCacheMatrix in the variable "m"

cacheSolve <- function(x, ...) {
        m <- x$getmatrix_inv()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setmatrix_inv(m)
        m
}


