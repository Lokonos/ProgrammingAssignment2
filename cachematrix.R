## This fucntions compute the inverse of a matrix and chache the results

## Creating a special "matrix" list
makeMatrix <- function (x=numeric()){
        i <- nrow(x)
        j <- ncol(x)
        mi <- matrix(NA,j,i)
        set <- function(y){
                x <<- y
                mi <<- matrix(NA,j,i)
        }
        get <- function() x
        setinverse <- function(matrixinverse) mi <<- matrixinverse
        getinverse <- function() mi
        list(set=set, get=get,setinverse=setinverse,getinverse=getinverse)
}


## This functions return the inverse on a matrix or get the cached value
cacheSolve <-function(x,...){
        mi <-x$getinverse()
        if(all(!is.na(mi))){
                message("getting cached data")
                return(mi)
        }
        data <- x$get()
        mi <- solve(data,...)
        x$setinverse(mi)
        mi
}
