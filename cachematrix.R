####THIS FIRST FUNCTION CREATES AN EMPTY MATRIX OBJECT THAT STORES ITS INVERSE'S CACHE####

makeCacheMatrix <- function(x = matrix()) {
        minv <- NULL ########### In case you are wandering, "minv" stands for "matriz invertida" which is "inverted matrix" in spanish. 
        set <- function(y) {
                x <<- y
                minv <<- NULL
        }
        get <- function() x
        mset <- function(solve) minv <<- solve #mset stands for "matrix set"
        mget <- function() minv #same principle here, msget stands for "matrix get"
        list(set=set, get=get,
             mset=mset,
             mget=mget)
}

#####THIS SECOND FUNCTION ACTUALLY COMPUTES THE INVERSE OF THE MATRIX PREVIOUSLY SPECIFIED IN THE LAST FUNCTION. IF SAID INVERSE HAS ALREADY BEEN CALCULATED
##### IT JUST GET'S THE DATA FROM THE CACHE, UNLESS OF COURSE THE ORIGINAL MATRIX WAS CHANGED IN THE MEANTIME

cacheSolve <- function(x, ...) {
        minv<-x$mget()
        if(!is.null(minv)){
                message("getting cached data")
                return(minv)
        }
        matrix<-x$get()
        minv<-solve(matrix, ...)
        x$mset(minv)
        minv
}

#######THIS IS A WAY TO CHECK IF THE FUNCTIONS WORK#############

mtest <- rbind(c(7, -4/5), c(-4/5, 7)) #########CREATE A TEST MATRIX##########
test <- makeCacheMatrix(mtest) ##########STORE THE RESULTS OF APPLYING THE FIRST FUNCTION IN "test" #############
test$get() ############RUN IT###########
cacheSolve(test) #############TEST THE SECOND FUNCTION. 

#IN ORDER TO SEE IF THE DATA WAS ACTUALLY STORED, YOU NEED TO RUN THE LAST LINE OF CODE TWICE AND THE SIGN "getting cached data" SHOULD APPEAR#################