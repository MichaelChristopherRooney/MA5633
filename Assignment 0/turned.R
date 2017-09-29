do_work <- function(n){
	mat<-getmatrix(n)
	mat<-turned(mat)
	return(mat)
}

# takes n and returns a nxn matrix
# filled with values 1:n
getmatrix <- function(n){
	size = n * n
	v <- c(1:size)
	mat <- matrix(v, ncol=n, nrow=n)
	return(mat)
}

# Rotate the matrix 90 degrees anti clockwise by first reversing the elements
# in each row, then transposing the matrix
turned <- function(x){
	if(!is.matrix(x)){
		warning("Expected a matrix")
		return(NA)
	}
	if(ncol(x) != nrow(x)){
		warning("Expected a square matrix")
		return(NA)
	}
	# Reverse the elements in each row
	# Uses bitwise XOR to swap two values without using a temp variable
	# See the "Swapping without "temp"" section on this page for details
	# http://www.cs.umd.edu/class/sum2003/cmsc311/Notes/BitOp/xor.html
	for(i in 1:nrow(x)){
		for(j in 1:(ncol(x) / 2)){
			cat("\ni: ", i)
			cat("\nj: ", j)
			cat("\nSwapping ", x[i, j])
			cat(" with ", x[i, nrow(x) - (j - 1)])
			x[i, j] <- bitwXor(x[i, j], x[i, nrow(x) - (j - 1)])
			x[i, nrow(x) - (j - 1)] <- bitwXor(x[i, nrow(x) - (j - 1)], x[i, j])
			x[i, j] <- bitwXor(x[i, j], x[i, nrow(x) - (j - 1)])
		}
	}
	# Does the R transpose implementation use extra space?
	# If so I should use my own implementation to conform to the assignment
	x <- t(x)
	return(x)
}
