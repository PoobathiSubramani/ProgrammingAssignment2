## This program inverts an input matrix and caches it
## checks for same matrix
## checks for square matrix


## this function gets matrix and sets inverted matrix

makeCacheMatrix <- function(x = matrix()) {
    
    if(dim(x)[1] != dim(x)[2]) {
        print("enter square matrix")
    }
    
    invm <- NULL #inverse matrix set to null
    
    getMatrix <- function() {
        x #returns the original matrix
    }
    
    setMatrix <- function(im = matrix()) { #takes an input matrix
        
        if(dim(im)[1] != dim(im)[2]) {
            print("enter square matrix")
            return()
        }
        
        if (is.matrix(x) == is.matrix(im) && dim(x) == dim(im) && all(x == im)) { #compares the two matrices
            print("similar matrix...not inverting again.")
        }
        else {
            print("new matrix entered")
            x <<- im #overrides the existing matrix
            invm <<- NULL #parent inverse matrix is set to null            
        }
    }
    
    setInvMatrix <- function(newmatrix = matrix()) { #sets a new matrix. This fn is only used by cacheSolve fn
        invm <<- newmatrix #parent inverted matrix is set with new matrix
    }
    
    getInvMatrix <- function() {
        invm #returns the temp matrix
    }
    
    list(getMatrix = getMatrix, getInvMatrix=getInvMatrix, setMatrix=setMatrix, setInvMatrix=setInvMatrix)
}


## this function computes inverse of input matrix and caches the results
## checks for existing cache to avoid redoing inversion

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    cim <- x$getInvMatrix() #cached inverted matrix
    if (!is.null(cim)) {
        print("cached inverted matrix")
        print(cim)
    }
    else {
        print("not cached.computing and caching...")
        im <- solve(x$getMatrix()) #compute inverted matrix
        x$setInvMatrix(im)
        im
    }
}
