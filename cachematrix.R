## makeCacheMatrix function will create a list of matrix objects
## This list can store the cache of inverse of a matrix once it is calculated.
## The inverse is retrieved from the cache if the function is called again with the same parameters


makeCacheMatrix <- function(x = matrix()) 
{
        matrInverse<-NULL
        setMatrix<-function(matr)
        {
                x <<- matr
                matrInverse <<- NULL
        }
        getMatrix<-function()
        {
                x
        }
        setInverse<-function(Invmatr)
        {
                matrInverse <<- Invmatr
        }
        getInverse<-function()
        {
                matrInverse
        }
        list(set=setMatrix, get=getMatrix,setInv=setInverse,getInv=getInverse)
}


## cacheSolve takes matrix as an input
## computes the inverse of the matrix returned by makeCacheMatrix function.
## If the inverse was already calculated, the cacheSolve fetches the inverse value thats already stored in the cache


cacheSolve <- function(x, ...) 
{
        ## Return a matrix that is the inverse of 'x'
        retList<-makeCacheMatrix(x)
        ## This loop is to demonstrate that when 2nd time the loop is executed
        ## Inverse of a matrix is fetched from the Cache
        for (i in 1:2)
        {
                
                matrInverse <- retList$getInv()
                if(!is.null(matrInverse))
                {
                        message("Iteration2: Get Matrix inverse from cached data")
                        return(matrInverse)
                }
                else
                {
                        message("Iteration1: No Cached Data. Calculate Matrix Inverse")
                }
                matr_data <- retList$get()
                matrInverse <- solve(matr_data)
                retList$setInv(matrInverse)
                matrInverse
        }
}
