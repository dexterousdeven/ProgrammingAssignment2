## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) 
{
    # initialize object with null value
    cache_matrix <- NULL
    
    # define 'set' function
    set_matrix <- function(y)
    {
        x <<- y
        cache_matrix <<- NULL
    }
    
    # define 'get' function
    get_matrix <- function() x
    
    # create inverse of matrix
    inverse_matrix <- function(solve) cache_matrix <<- solve
    
    # define other 'get' function
    get_inverse <- function() cache_matrix
    
    # return list of functions
    list(set_matrix = set_matrix,
         get_matrix = get_matrix,
         inverse_matrix = inverse_matrix,
         get_inverse = get_inverse
         )
}

## Write a short comment describing this function

cacheSolve <- function(x, ...) 
{
    ## Return a matrix that is the inverse of 'x'
    
    # get matrix value
    cache_matrix <- x$get_inverse()
    
    # check for pre-existing value
    if(!is.null(cache_matrix))
    {
        message("getting cached matrix data")
        return(cache_matrix)
    }
    
    # return inverse if null
    data <- x$get_matrix()
    
    cache_matrix <- solve(data)
    
    x$inverse_matrix(cache_matrix)
    
    # return matrix
    return(cache_matrix)
}

