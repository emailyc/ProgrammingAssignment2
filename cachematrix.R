## The functions recieves a matrix and attempts to calaulate an inverse of it. 
#The functions then cache the result in memory. So instead of retrieving the inversed matrix by 
#running a calculation everytime it's called, the instance of one of the function has a method whcih 
#retrieves the cached result from memory. This is made possible by lexical scoping which 
#symols inside a function look for it's value in the environment where the function is defined.

## makeCacheMatrix recieves a matrix as argument. The function returns four functions 
#which are getters and setters. Instance of makeCacheMatrix can assign/change the value x and m
# by calling these four methods.

makeCacheMatrix <- function(x = matrix()) {
    
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() {x}
    setmatrix <- function(matrix) {m <<- matrix}
    getmatrix <- function() {
        if (!is.null(m)) {
            print("getting cached data from cacheSolve")
        }
        m
        }
    list(set = set, get = get,
         setmatrix = setmatrix,
         getmatrix = getmatrix)
    
}


## cacheSolve takes an instance of the makeCacheMatrix function as it's argument.
#cacheSolve checks whether m has a value. If m has a value, cacheSolve retrieves m's
#value from memory. If m is null, then cacheSolve calls the methods of the makeCacheMatrix instance
#which are getters and setters of x and m. 
#m (the inversed matrix) is returned at the end of cacheSolve. And also alters/assign 
#value to the m symbol in the makeCacheMatrix instance.

cacheSolve <- function(x, ...) {
    
    
    m <- x$getmatrix()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setmatrix(m)
    m ## Return a matrix that is the inverse of 'x'
}
