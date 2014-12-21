## This is ProgrammingAssignment2 solution from Hindrek Jermann
## Function calculates inverse of matrix

## This function is constructor for building cached matrix
makeCacheMatrix <- function(x = matrix()) {
        m<-NULL
        set<-function(y){
                x<<-y
                m<<-NULL
        }
        get<-function() x
        setmatrix<-function(solve) m<<- solve
        getmatrix<-function() m
        list(set=set, get=get,
             setmatrix=setmatrix,
             getmatrix=getmatrix)
}

## creating cache object
csh<<-makeCacheMatrix()

## Inversing matrix or using cached version, if the provided
## input is same as in cache

cacheSolve <- function(y=matrix(), ...) {
        
        cs<- csh$get()
        
        if(identical(y,cs)){
                m<-csh$getmatrix()
                if(!is.null(m)){
                        message("getting cached data")
                        return(m)   
                } else {
                        m<-solve(y, ...)
                        csh$setmatrix(m)
                        return(m)
                }
                
        } else {
                m<-solve(y, ...)
                csh$set(y)
                csh$setmatrix(m)
                return(m) 
        }       
}




