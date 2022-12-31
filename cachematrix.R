makeCacheMatrix <- function(x = matrix()){ #creates a special matrix
        i <- NULL # holds inverse
        set <- function(y){ #sets value for matrix
                x <<- y
                i <<- NULL
        }
        get <- function()x # gets value of matrix
        setinverse <- function(solve) i <<- solve # sets value of inverse
        getinverse <- function()i # gets value of invers
        list(set = set, get = get, setinverse = setinverse, 
             getinverse = getinverse) # list to get $ properties
}

cacheSolve <- function(x, ...){ # calculates inverse from above function
        i <- x$getinverse() # Gets cached calue of inversed matrix
        if(!is.null(i)){ # if cached value exsits return it
                message("getting cached data") # notifies user
                return(i) # returns inverse matrix
        }
        data <- x$get() # get matrix to inverse
        i <- solve(data, ...) # inverse matrix
        x$setinverse(i) # store result in makeCacheMatrix
        i # return inverse matrix
}

