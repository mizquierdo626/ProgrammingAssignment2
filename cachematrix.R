##Functions that cache the inverse of a matrix##



#This function computes the inverse of the special "matrix" object 
#that can cache its inverse
makeCacheMatrix <- function(x = matrix())   {
	#Initialize the inverse property
		inv <- NULL

	#Set the matrix
		set <- function(matrix)				{
			x <<- matrix
			inv <<- NULL
}	
	#Get the matrix
		get <- function() 
	#Return the matrix
		x
	#Set the inverse of the matrix
		setInverse <- function(inverse) 
			inv <<- inverse
	#Get the inverse of the matrix
		getInverse <- function() 
	#Return the inverse property
		inv
	#Return the list of the functions
		list(set = set, get = get, 
			 setInverse = setInverse,
			 getInverse = getInverse)
}


#This function computes the inverse of the special "matrix" return by "makeCacheMatrix" above
#--If the inverse has already been calculated (matrix not changed) then "cacheSolve" 
#--Should retrieve it from the cache
cacheSolve <- function(x, ...) 				{
    #Return a matrix that is the inverse of 'x'
    	inv <- x$getInverse()
    #Return the inverse if its already set
    	if(!is.null(inv)) 					{
    		message("getting cached data")
    		return(inv)
}
	#Get the matrix from the special object
		mat <- x$get()
	#Calculate the inverse using matrix multiplaction
		inv <- solve(mat, ...)
	#Set the inverse to the object
		x$setInverse(inv)
	#Return the matrix
		inv

}



























