## Put comments here that give an overall description of what your
## functions do
## There are two functions in this programm
## firt fn is makeCacheMatrix. It does four tasks using four function when called upon..
## 1. Set the matrix passed by argument to variable x
## 2. Get the matrix if already exixt otherwise returns null
## 3. set the value of inverse
## 4. get the value of inverse of matrix if calculated

makeCacheMatrix <- function(x = matrix()) {
        n <- NULL
        set <- function(y)
        {
            x <<- y
            n <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) n <<- solve 
        getinverse <- function() n
        
        list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
        
}


## Write a short comment describing this function
## here actual inverse of matrix is calculated using solve() function 
## if inverse of same matrix is already taken then solve() function is not called...
## ... and value is taken form cache(value stored in previous variable)

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        n <- x$getinverse()
        if(!is.null(n))
        {
          message("For this matrix inverse has been calculated before. No need to calculate again. getting catched data...")
          return(n)
        }
        data <- x$get()
        n <- solve(data,...)
        x$setinverse(n)
        n
}
